#pragma once

/*
class QRhi;
class QRhiBuffer;
class QRhiResourceUpdateBatch;
class QRhiCommandBuffer;
*/

#pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop

extern QRhi *g_rhi;


namespace r
{

struct TriangleVertex
{
    float x, y;     // position
    float r, g, b, a; // color
};

struct TextureVertex
{
    float x, y;     // position (on screen)
    float u, v;     // texture coordinates (position in the texture atlas)
    float r, g, b, a; // color
};


namespace vertices
{

struct Buffer
{
	int _buffer_pos = 0;
	
	int _buffer_size;
	float *_buffer;

	~Buffer()
	{
		free(_buffer);
	}

	Buffer(int initial_size)
		: _buffer_size(initial_size)
		, _buffer((float*)malloc(sizeof(float) * _buffer_size))
	{
		memset(_buffer, 0, sizeof(float) * _buffer_size); // Ensure it's allocated before starting to use it (calloc might not always actually allocate.)
	}
	
	void append(const float *data, int num_floats)
	{
		if (_buffer_pos + num_floats > _buffer_size)
		{
			do
			{
				_buffer_size *= 2;
			}
			while (_buffer_pos + num_floats > _buffer_size);
						
			_buffer = (float*)realloc(_buffer, _buffer_size * sizeof(float));
		}

		memcpy(_buffer + _buffer_pos, data, num_floats * sizeof(float));

		_buffer_pos += num_floats;
	}

	void reset(void)
	{
		_buffer_pos = 0;
	}	
};

struct Buffers
{
	radium::Mutex _lock;

	// Triple-buffered: at most {_next_ready_buffer, _committed_buffer} are occupied as distinct
	// buffers at the same time (_qrhi_buffer is always == _committed_buffer while held), so there
	// is always at least one free buffer for the main thread to render into.
	Buffer _buffers[3];

	int _render_buffer     = -1; // -1, 0, 1 or 2. Main thread is generating into this.
	int _next_ready_buffer = -1; // published by main, awaiting commit
	int _committed_buffer  = -1; // committed to the current qrhi frame.
	int _qrhi_buffer       = -1; // transient: being uploaded (always == _committed while set).

	bool _committed_is_fresh = false; // If true then the committed buffer needs (re)upload.

	Buffers(int initial_size)
		: _buffers{initial_size, initial_size, initial_size}
	{
	}
	
	// Called by the qrhi thread at the top of a frame.
	// Snapshots the latest published buffer as the one this frame (and following
	// frames, until the next commit) will use.
	void QRHI_commit(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		radium::ScopedMutex lock(_lock);

		if (_next_ready_buffer != -1)
		{
			_committed_buffer = _next_ready_buffer;
			_next_ready_buffer = -1;
			_committed_is_fresh = true;
		}
	}

	[[nodiscard]] const Buffer *QRHI_maybe_obtain_qrhi_buffer(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		radium::ScopedMutex lock(_lock);
		
		R_ASSERT_NON_RELEASE(_qrhi_buffer == -1);

		// Only re-upload when a freshly committed buffer is available; otherwise the
		// existing GPU vertex buffer is still valid for this frame.
		if (_committed_is_fresh && _committed_buffer != -1)
		{
			_qrhi_buffer = _committed_buffer;
			_committed_is_fresh = false;

			R_ASSERT_NON_RELEASE(_render_buffer != _qrhi_buffer);

			return &_buffers[_qrhi_buffer];
		}

		return NULL;
 	}

	void QRHI_release_qrhi_buffer(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		radium::ScopedMutex lock(_lock);

		R_ASSERT_NON_RELEASE(_qrhi_buffer != -1);
		
		_qrhi_buffer = -1;
	}
	
	[[nodiscard]] Buffer *MAIN_obtain_render_buffer(void) __attribute__((returns_nonnull))
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		radium::ScopedMutex lock(_lock);

		if (_render_buffer != -1)
		{
			// Already holding a render buffer (startup races can call start twice).
			fprintf(stderr, "WARN: obtain_render_buffer called while _render_buffer=%d, returning existing buffer (Buffers@%p)\n",
				    _render_buffer,
					(void*)this);
			return &_buffers[_render_buffer];
		}

		// Pick any buffer not currently owned by the render thread. At most two buffers
		// should be used by the qrhi thread, so one should always be free.
		_render_buffer = -1;
		for (int i = 0; i < 3; i++)
			if (i != _next_ready_buffer && i != _committed_buffer && i != _qrhi_buffer)
			{
				_render_buffer = i;
				break;
			}

		if (_render_buffer == -1)
		{
			R_ASSERT_NON_RELEASE(false);
			_render_buffer = 0;
		}

		Buffer *ret = &_buffers[_render_buffer];

		ret->reset();
		
		return ret;
	}

	// Called from main thread. Publishes the render buffer so that the RHI thread
	// can commit it on the next frame.
	void MAIN_release_render_buffer(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		radium::ScopedMutex lock(_lock);

		if (_render_buffer == -1)
			return; // Already released (e.g. frame-skip or mismatched start/release)

		// A previous, not-yet-committed ready buffer is superseded and
		// becomes free again.
		_next_ready_buffer = _render_buffer;
		_render_buffer = -1;
	}
};
	
} // namespace vertices


template <int VERTEX_SIZE, int NUM_VERTICES_PER_UNIT>
struct Vertices
{
	static constexpr int START_SIZE = 128; // Initial number of units.

	static constexpr int _num_vertices_per_unit = NUM_VERTICES_PER_UNIT;
	static constexpr int _vertex_size = VERTEX_SIZE;
	
	//const int _vertex_size;
	
	QRhiBuffer *_vbuf = nullptr;

	int _curr_num_qhri_vertices = -1;
	
	vertices::Buffers _buffers;
	
	vertices::Buffer *_render_buffer;
	
	Vertices() //int vertex_size, int num_vertices_per_unit)
		: _buffers(sizeof(float) * VERTEX_SIZE * NUM_VERTICES_PER_UNIT * START_SIZE)
	{	
	}

	~Vertices()
	{
		delete _vbuf;
	}

	void QRHI_maybe_merge_in(QRhiResourceUpdateBatch *update_batch)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		const vertices::Buffer *buffer = _buffers.QRHI_maybe_obtain_qrhi_buffer();

		if (buffer==NULL)
			return;

		const int num_bytes = buffer->_buffer_pos * sizeof(float);
		
		if (_vbuf == NULL || (int)_vbuf->size() < num_bytes)
		{
			const int size = std::max(num_bytes + 1024,
									  _vbuf == NULL ? 1024 : int(_vbuf->size()) * 2);
			
			delete _vbuf;

			_vbuf = g_rhi->newBuffer(QRhiBuffer::Dynamic,
									 QRhiBuffer::VertexBuffer,
									 size);
		}
				
		if (!_vbuf || !_vbuf->create())
		{
			GFX_Message(NULL, "Failed to create vertex buffer");
		}

		update_batch->updateDynamicBuffer(_vbuf,
										  0,
										  num_bytes,
										  buffer->_buffer);

		_curr_num_qhri_vertices = buffer->_buffer_pos / VERTEX_SIZE;

		//printf("Updated vbuf. Num bytes: %d. Num vertices: %d\n", num_bytes, _curr_num_qhri_vertices);
		//getchar();
		 
		_buffers.QRHI_release_qrhi_buffer();
	}
	
	void QRHI_render(QRhiCommandBuffer *command_buffer)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		if (_vbuf && _curr_num_qhri_vertices>0)
		{
			const QRhiCommandBuffer::VertexInput vbufBinding(_vbuf, 0);
			command_buffer->setVertexInput(0, 1, &vbufBinding);
			command_buffer->draw(_curr_num_qhri_vertices);
		}
	}

	bool QRHI_has_vertices(void) const
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		return _vbuf && _curr_num_qhri_vertices > 0;
	}

	void MAIN_call_me_when_starting_to_generate_vertices(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		_render_buffer = _buffers.MAIN_obtain_render_buffer();
	}
	
	void MAIN_call_me_after_finished_generating_vertices(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		_buffers.MAIN_release_render_buffer();
	}

	// Called from RHI thread. Snapshots the latest published buffer as the
	// committed buffer for this frame.
	void QRHI_commit_buffers(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());
		_buffers.QRHI_commit();
	}
	
	void MAIN_addUnit(const float *unit)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		_render_buffer->append(unit, VERTEX_SIZE * NUM_VERTICES_PER_UNIT);
	}

	
	void MAIN_addUnit(std::initializer_list<float> list)
	{
		//fprintf(stderr,"Size: %d\n", (int)(list.end()-list.begin()));
		R_ASSERT_NON_RELEASE((list.end() - list.begin()) == VERTEX_SIZE * NUM_VERTICES_PER_UNIT);
		
		MAIN_addUnit(list.begin());
	}
};

struct TriangleVertices : public Vertices<sizeof(TriangleVertex)/sizeof(float), 3>
{
	static_assert(_vertex_size == 6);
	
	void MAIN_addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a,
					 float r2, float g2, float b2, float a2,
					 float r3, float g3, float b3, float a3)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		float f[_num_vertices_per_unit * _vertex_size];
		
#define POS(I, X, Y)							\
		f[I] = X;								\
		f[I+1] = Y;

#define COL(I, R, G, B, A)						\
		f[I] = R;								\
		f[I+1] = G;								\
		f[I+2] = B;								\
		f[I+3] = A;
		
		// P1
		POS(0, x1, y1);
		COL(2, r, g, b, a);
		//printf("P1. In: %f, %f. Out: %f, %f\n", x1, y1, f[0], f[1]);
		
		// P2
		POS(6, x2, y2);
		COL(8, r2, g2, b2, a2);
		//printf("P2. In: %f, %f. Out: %f, %f\n", x2, y2, f[6], f[7]);
		
		// P3
		POS(12, x3, y3);
		COL(14, r3, g3, b3, a3);
		//printf("P3. In: %f, %f. Out: %f, %f\n", x3, y3, f[14], f[15]);
		
		MAIN_addUnit(f);
	}
	
	void MAIN_addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a = 0.01)
	{
		MAIN_addTriangle(x1, y1, x2, y2, x3, y3, r, g, b, a, r, g, b, a, r, g, b, a);
	}
					 
	void MAIN_add_triangle(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3,
					  GE_Rgb rgb)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		MAIN_addTriangle(p1.a, p1.b,
					p2.a, p2.b,
					p3.a, p3.b,
					(float)rgb.r / 256.0f, (float)rgb.g / 256.0f, (float)rgb.b / 256.0f, (float)rgb.a / 256.0f);
	}

	void MAIN_add_triangle(const r::Triangle &triangle, GE_Rgb rgb)
	{
		MAIN_add_triangle(triangle._v0, triangle._v1, triangle._v2, rgb);
	}

	void MAIN_add_triangle_horizontal_gradient(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3,
											   GE_Rgb rgb, GE_Rgb rgb2)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		// Compute left-to-right blend factor for each vertex based on x position
		float x1 = p1.a;
		float x2 = p2.a;
		float x3 = p3.a;
		
		float minx = R_MIN(x1, R_MIN(x2, x3));
		
		float maxx = R_MAX(x1, R_MAX(x2, x3));
		
		float dx = maxx - minx;

		auto lerp = [](float a, float b, float t)
		{
			return a + (b - a) * t;
		};
		
		auto clamp01 = [](float v)
		{
			if (v < 0.0f)
				return 0.0f;
			if (v > 1.0f)
				return 1.0f;
			else
				return v;
		};

		float t1 = equal_floats(dx, 0.0f) ? 0.0f : clamp01((x1 - minx) / dx);
		float t2 = equal_floats(dx, 0.0f) ? 0.0f : clamp01((x2 - minx) / dx);
		float t3 = equal_floats(dx, 0.0f) ? 0.0f : clamp01((x3 - minx) / dx);

		float r1 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t1);
		float g1 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t1);
		float b1 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t1);
		float a1 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t1);

		float r2 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t2);
		float g2 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t2);
		float b2 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t2);
		float a2 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t2);

		float r3 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t3);
		float g3 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t3);
		float b3 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t3);
		float a3 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t3);

		MAIN_addTriangle(p1.a, p1.b,
					p2.a, p2.b,
					p3.a, p3.b,
					r1, g1, b1, a1,
					r2, g2, b2, a2,
					r3, g3, b3, a3);
	}

	void MAIN_add_triangle_horizontal_gradient(const r::Triangle &triangle, GE_Rgb rgb, GE_Rgb rgb2)
	{
		MAIN_add_triangle_horizontal_gradient(triangle._v0, triangle._v1, triangle._v2, rgb, rgb2);
	}

	void MAIN_add_triangle_vertical_gradient(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3,
										GE_Rgb rgb, GE_Rgb rgb2)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		// Distribute gradient top-to-bottom (y increases downwards)
		float y1 = p1.b;
		float y2 = p2.b;
		float y3 = p3.b;
		
		float miny = R_MIN(y1, R_MIN(y2, y3));
		
		float maxy = R_MAX(y1, R_MAX(y2, y3));
		
		float dy = maxy - miny;

		auto lerp = [](float a, float b, float t)
		{
			return a + (b - a) * t;
		};
		
		auto clamp01 = [](float v)
		{
			if (v < 0.0f)
				return 0.0f;
			if (v > 1.0f)
				return 1.0f;
			else
				return v;
		};

		float t1 = equal_floats(dy, 0.0f) ? 0.0f : clamp01((y1 - miny) / dy);
		float t2 = equal_floats(dy, 0.0f) ? 0.0f : clamp01((y2 - miny) / dy);
		float t3 = equal_floats(dy, 0.0f) ? 0.0f : clamp01((y3 - miny) / dy);

		float r1 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t1);
		float g1 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t1);
		float b1 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t1);
		float a1 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t1);

		float r2 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t2);
		float g2 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t2);
		float b2 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t2);
		float a2 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t2);

		float r3 = lerp((float)rgb.r / 256.0f, (float)rgb2.r / 256.0f, t3);
		float g3 = lerp((float)rgb.g / 256.0f, (float)rgb2.g / 256.0f, t3);
		float b3 = lerp((float)rgb.b / 256.0f, (float)rgb2.b / 256.0f, t3);
		float a3 = lerp((float)rgb.a / 256.0f, (float)rgb2.a / 256.0f, t3);

		MAIN_addTriangle(p1.a, p1.b,
					p2.a, p2.b,
					p3.a, p3.b,
					r1, g1, b1, a1,
					r2, g2, b2, a2,
					r3, g3, b3, a3);
	}

	void MAIN_add_triangle_vertical_gradient(const r::Triangle &triangle, GE_Rgb rgb, GE_Rgb rgb2)
	{
		MAIN_add_triangle_vertical_gradient(triangle._v0, triangle._v1, triangle._v2, rgb, rgb2);
	}
};


struct TextureVertices : public Vertices<sizeof(TextureVertex)/sizeof(float), 6> // (6 vertices == 2 triangles == 1 square)
{
	static_assert(_vertex_size == 8);

	void MAIN_addTexture(float x1, float y1, float x2, float y2, // dest square
					float u0, float v0, float u1, float v1, // source square
					float r, float g, float b, float a
		)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		#if 0
		printf("Add-texture: Dest: %f,%f -> %f,%f\n"
			   "             Src:  %f,%f -> %f,%f\n"
			   "             r: %f. g: %f. b: %f. a: %f\n\n",
			   x1,y1,x2,y2,
			   u0,v0,u1,v1,
			   r,g,b,a);
		#endif

		float f[_num_vertices_per_unit * _vertex_size];

#define ADD_VERTEX(I, X, Y, uv_x, uv_y)									\
		f[I] = X ; f[I+1] = Y ; f[I+2] = uv_x ; f[I+3] = uv_y ;			\
		f[I+4] = r ; f[I+5] = g ; f[I+6] = b ; f[I+7] = a;

		// Triangle 1: y2-x1, y2-x2, y1-x2
		ADD_VERTEX(0, x1, y2, u0, v1);
		ADD_VERTEX(8, x2, y2, u1, v1);
        ADD_VERTEX(16, x2, y1, u1, v0);
		
        // Triangle 2: y2-x1, y1-x2, y1-x1
        ADD_VERTEX(24, x1, y2, u0, v1);
        ADD_VERTEX(32, x2, y1, u1, v0);
        ADD_VERTEX(40, x1, y1, u0, v0);

		MAIN_addUnit(f);
	}
};

}
