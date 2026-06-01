
// TODO: Rename Context -> Vertices.

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
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

	Buffer _buffers[2];

	int _render_buffer = -1; // either -1, 0, or 1.
	int _qrhi_buffer = -1; // either -1, 0, or 1.

	int _next_ready_buffer = -1; // either -1, 0, or 1.

	Buffers(int initial_size)
		: _buffers{initial_size, initial_size}
	{
	}
	
	[[nodiscard]] const Buffer *maybe_obtain_qrhi_buffer(void)
	{
		radium::ScopedMutex lock(_lock);
		
		R_ASSERT_NON_RELEASE(_qrhi_buffer == -1);

		if (_next_ready_buffer != -1)
		{
			_qrhi_buffer = _next_ready_buffer;
			_next_ready_buffer = -1;
			
			R_ASSERT_NON_RELEASE(_render_buffer != _qrhi_buffer);
			
			return &_buffers[_qrhi_buffer];
		}

		return NULL;
 	}

	void release_qrhi_buffer(void)
	{
		radium::ScopedMutex lock(_lock);

		R_ASSERT_NON_RELEASE(_qrhi_buffer != -1);
		
		_qrhi_buffer = -1;
	}
	
	[[nodiscard]] Buffer *obtain_render_buffer(void) __attribute__((returns_nonnull))
	{
		radium::ScopedMutex lock(_lock);

		R_ASSERT_NON_RELEASE(_render_buffer == -1);

		switch(_qrhi_buffer)
		{
			case -1:
				switch(_next_ready_buffer)
				{
					case -1: _render_buffer = 0 ; break;
					case 0: _render_buffer = 1 ; break; // Probably best, but should we return 0 instead? (Both will behave correctly, but which one looks best? In theory, 0 might have lower latency, while 1 also might have lower latency in addition to smoother animation. It might not be possible to notice any difference though.)
					case 1: _render_buffer = 0 ; break; // Probably best, but should we return 1 instead? (...)
					default:
						_render_buffer = 0;
						R_ASSERT_NON_RELEASE(false);
						break;
				}
				break;
			case 0: _render_buffer = 1 ; break;
			case 1: _render_buffer = 0 ; break;
			default:
				R_ASSERT_NON_RELEASE(false);
				_render_buffer = 0;
				break;
		}

		Buffer *ret = &_buffers[_render_buffer];

		ret->reset();
		
		return ret;
	}

	void release_render_buffer(void)
	{
		radium::ScopedMutex lock(_lock);

		R_ASSERT_NON_RELEASE(_render_buffer != -1);

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

	void maybe_merge_in(QRhiResourceUpdateBatch *update_batch)
	{
		const vertices::Buffer *buffer = _buffers.maybe_obtain_qrhi_buffer();

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
				
		if (!_vbuf || !_vbuf->create()) {
			qDebug() << "Failed to create vertex buffer";
			getchar();
		}

		update_batch->updateDynamicBuffer(_vbuf,
										  0,
										  num_bytes,
										  buffer->_buffer);

		_curr_num_qhri_vertices = buffer->_buffer_pos / VERTEX_SIZE;

		printf("Updated vbuf. Num bytes: %d. Num vertices: %d\n", num_bytes, _curr_num_qhri_vertices);
		//getchar();
		 
		_buffers.release_qrhi_buffer();
	}
	
	void render(QRhiCommandBuffer *command_buffer)
	{
		if (_vbuf && _curr_num_qhri_vertices>0)
		{
			const QRhiCommandBuffer::VertexInput vbufBinding(_vbuf, 0);
			command_buffer->setVertexInput(0, 1, &vbufBinding);
			command_buffer->draw(_curr_num_qhri_vertices);
		}
	}

	void call_me_when_starting_to_generate_vertices(void)
	{
		_render_buffer = _buffers.obtain_render_buffer();
	}
	
	void call_me_after_finished_generating_vertices(void)
	{
		_buffers.release_render_buffer();
	}
	
	void addUnit(const float *unit)
	{
		_render_buffer->append(unit, VERTEX_SIZE * NUM_VERTICES_PER_UNIT);
	}

	
	void addUnit(std::initializer_list<float> list)
	{
		//fprintf(stderr,"Size: %d\n", (int)(list.end()-list.begin()));
		assert((list.end() - list.begin()) == VERTEX_SIZE * NUM_VERTICES_PER_UNIT);
		addUnit(list.begin());
	}
};

struct TriangleVertices : public Vertices<sizeof(TriangleVertex)/sizeof(float), 3>
{
	static_assert(_vertex_size == 6);
	
	void addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a,
					 float r2, float g2, float b2, float a2)
	{
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
		COL(8, r, g, b, a);
		//printf("P2. In: %f, %f. Out: %f, %f\n", x2, y2, f[6], f[7]);
		
		// P3
		POS(12, x3, y3);
		COL(14, r2, g2, b2, a2);
		//printf("P3. In: %f, %f. Out: %f, %f\n", x3, y3, f[14], f[15]);
		
		addUnit(f);
	}
	
	void addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a = 0.01)
	{
		addTriangle(x1, y1, x2, y2, x3, y3, r, g, b, a, r, g, b, a);
	}
					 
	void add_triangle(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3,
					  GE_Rgb rgb)
	{
		addTriangle(p1.a, p1.b,
					p2.a, p2.b,
					p3.a, p3.b,
					(float)rgb.r / 256.0f, (float)rgb.g / 256.0f, (float)rgb.b / 256.0f, (float)rgb.a / 256.0f);
	}

	void add_triangle(const r::fvec2 &p1, const r::fvec2 &p2, const r::fvec2 &p3,
					  GE_Rgb rgb, GE_Rgb rgb2)
	{
		addTriangle(p1.a, p1.b,
					p2.a, p2.b,
					p3.a, p3.b,
					(float)rgb.r / 256.0f, (float)rgb.g / 256.0f, (float)rgb.b / 256.0f, (float)rgb.a / 256.0f,
					(float)rgb2.r / 256.0f, (float)rgb2.g / 256.0f, (float)rgb2.b / 256.0f, (float)rgb2.a / 256.0f);
	}
};


struct TextureVertices : public Vertices<sizeof(TextureVertex)/sizeof(float), 6> // (6 vertices == 2 triangles == 1 square)
{
	static_assert(_vertex_size == 8);

	void addTexture(float x1, float y1, float x2, float y2, // dest square
					float u0, float v0, float u1, float v1, // source square
					float r, float g, float b, float a
		)
	{
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

		addUnit(f);
	}
};

}

extern r::TriangleVertices *g_vertices_under_text; // Scrolling: Everything painted under text (only the track backgruond color.)
extern r::TriangleVertices *g_vertices_text; // Scrolling: Text.
extern r::TriangleVertices *g_vertices; // Scrolling: Everything painted above text)
extern r::TriangleVertices *g_vertices_left_slider; // Left slider (Scrolls it's own way)
extern r::TriangleVertices *g_vertices_static; // Non-Scrolling: Cursor + Indicators

//extern QVector<r::Vertices> g_all_verticess = {};

#define ALL_VERTICESS {g_vertices_under_text, g_vertices_text, g_vertices, g_vertices_left_slider, g_vertices_static}




