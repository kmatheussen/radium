
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop

extern QRhi *g_rhi;

namespace r
{
struct Context
{
	static constexpr int START_SIZE = 200; // Number of triangles
	static constexpr int TRIANGLE_SIZE = 18; // Number of floats in one triangle (pos1 + pos1-color, pos2 + pos2-color, pos3 + pos3-color.)

	int _buffer_size = TRIANGLE_SIZE * 200;
	int _buffer_pos = 0;
	float *_buffer = (float*)malloc(_buffer_size * sizeof(float));

	QVector<QRhiBuffer*> _buffers_to_delete;
	radium::Mutex _vbuf_lock;
	
	QRhiBuffer *_vbuf = nullptr;

	QRhiResourceUpdateBatch *_updates = nullptr;
	
	Context()
	{	
	}

	~Context()
	{
		delete _vbuf;
		free(_buffer);
	}

	std::function<void(void)> _do_before_merging;
	
	void call_me_when_finished_painting(QRhi *rhi)
	{
		static int total = 0;
		total += get_num_bytes();
		printf("Num bytes: %d. Total: %d\n", get_num_bytes(), total);

		radium::ScopedMutex lock(_vbuf_lock);
		
		_do_before_merging = [rhi, this](void)
			{
				//radium::ScopedMutex lock(_vbuf_lock);

				for(auto *buf : _buffers_to_delete)
					delete buf;
		
				_buffers_to_delete.clear();
				
				_vbuf = rhi->newBuffer(QRhiBuffer::Static, // Note: Possible optimization here. Don't really understand difference between Static, Immutable, and Dynamic.
									   QRhiBuffer::VertexBuffer,
									   get_num_bytes());
				
				_vbuf->create();

				if (_updates != nullptr)
				{
					_updates->release(); // unused.
				}
		
				_updates = rhi->nextResourceUpdateBatch();
				_updates->uploadStaticBuffer(_vbuf, get_buffer());
			};
	}
	
	void maybe_merge_in(QRhiResourceUpdateBatch *update_batch)
	{
		radium::ScopedMutex lock(_vbuf_lock);
		
		//if (_updates)
		if (_do_before_merging)
		{
			_do_before_merging();
			_do_before_merging = nullptr;
			
			printf("MERGING UPDATES\n");
			update_batch->merge(_updates);
			_updates->release();
			_updates = nullptr;
		}
	}

	void render(QRhiCommandBuffer *cb)
	{
		radium::ScopedMutex lock(_vbuf_lock);
		
		if (_vbuf)
		{
			const QRhiCommandBuffer::VertexInput vbufBinding(_vbuf, 0);
			cb->setVertexInput(0, 1, &vbufBinding);
			cb->draw(get_num_vertices());
		}
	}
	
	void clear(void)
	{
		/*
		_buffer_size = TRIANGLE_SIZE * 200 * sizeof(float);
		_buffer_pos = 0;
		_buffer = (float*)malloc(_buffer_size);
		*/
		_buffer_pos = 0;

		radium::ScopedMutex lock(_vbuf_lock);

		if (_vbuf != NULL)
		{
			_buffers_to_delete.push_back(_vbuf);
			_vbuf = NULL;
		}
	}
	
	void addTriangle(const float *triangle)
	{
		if (_buffer_pos+TRIANGLE_SIZE >= _buffer_size)
		{
			_buffer_size *= 2;
			_buffer = (float*)realloc(_buffer, _buffer_size * sizeof(float));
		}

		memcpy(_buffer + _buffer_pos, triangle, TRIANGLE_SIZE * sizeof(float));

		_buffer_pos += TRIANGLE_SIZE;
	}

	
	void addTriangle(std::initializer_list<float> list)
	{
		//fprintf(stderr,"Size: %d\n", (int)(list.end()-list.begin()));
		assert((list.end() - list.begin()) == TRIANGLE_SIZE);
		addTriangle(list.begin());
	}

	void addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a,
					 float r2, float g2, float b2, float a2)
	{
		float f[TRIANGLE_SIZE];

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
		
		addTriangle(f);
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

	int get_num_bytes(void) const
	{
		return _buffer_pos * sizeof(float);
	}

	int get_num_vertices(void) const
	{
		return _buffer_pos * 3 / TRIANGLE_SIZE;
	}

	const float *get_buffer(void) const
	{
		return _buffer;
	}	
};

}

extern r::Context *g_context_under_text; // Scrolling: Everything painted under text (only the track backgruond color.)
extern r::Context *g_context_text; // Scrolling: Text.
extern r::Context *g_context; // Scrolling: Everything painted above text)
extern r::Context *g_context_left_slider; // Left slider (Scrolls it's own way)
extern r::Context *g_context_static; // Non-Scrolling: Cursor + Indicators

//extern QVector<r::Context> g_all_contexts = {};

#define ALL_CONTEXTS {g_context_under_text, g_context_text, g_context, g_context_left_slider, g_context_static}




