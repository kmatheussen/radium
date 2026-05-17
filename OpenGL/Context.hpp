
// TODO: Rename Context -> Vertices.

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop

extern QRhi *g_rhi;

namespace r
{

template <int VERTEX_SIZE>
struct Context
{
	static constexpr int START_SIZE = 200; // Number of triangles

	int _buffer_size = VERTEX_SIZE * START_SIZE;
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

	//static constexpr int START_SIZE = 200; // Number of triangles
	//static constexpr int VERTEX_SIZE = 18; // Number of floats in one triangle (pos1 + pos1-color, pos2 + pos2-color, pos3 + pos3-color.)

	std::function<void(QRhiResourceUpdateBatch*)> _do_before_merging;
	
	void call_me_when_finished_painting(QRhi *rhi)
	{
		static int total = 0;
		total += get_num_bytes();

		if (VERTEX_SIZE != 18)
			printf("Num bytes: %d. Total: %d\n", get_num_bytes(), total);

		radium::ScopedMutex lock(_vbuf_lock);
		
		_do_before_merging = [rhi, this](QRhiResourceUpdateBatch *update_batch)
			{
				//radium::ScopedMutex lock(_vbuf_lock);

				for(auto *buf : _buffers_to_delete)
					delete buf;
		
				_buffers_to_delete.clear();

				if (VERTEX_SIZE != 18)
					printf("   NEWBUFFER: %d\n", get_num_bytes());

				_vbuf = rhi->newBuffer(QRhiBuffer::Dynamic, //Static, // Note: Possible optimization here. Don't really understand difference between Static, Immutable, and Dynamic.
									   QRhiBuffer::VertexBuffer,
									   get_num_bytes());

				if (!_vbuf || !_vbuf->create()) {
					qDebug() << "Failed to create vertex buffer";
					getchar();
				}

				//update_batch = rhi->nextResourceUpdateBatch();
				
				update_batch->updateDynamicBuffer(_vbuf,
												  0,
												  get_num_bytes(),
												  get_buffer());
			};
	}
	
	void maybe_merge_in(QRhiResourceUpdateBatch *update_batch)
	{
		radium::ScopedMutex lock(_vbuf_lock);
		
		//if (_updates)
		if (_do_before_merging)
		{
			_do_before_merging(update_batch);
			_do_before_merging = nullptr;
			
			printf("MERGING UPDATES\n");
			update_batch->merge(update_batch);
			/*
			_updates->release();
			_updates = nullptr;
			*/
		}
	}

	void clear(void)
	{
		/*
		_buffer_size = VERTEX_SIZE * 200 * sizeof(float);
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

	void render(QRhiCommandBuffer *command_buffer)
	{
		radium::ScopedMutex lock(_vbuf_lock);

		int num_vertices = get_num_vertices();

		if (false) //VERTEX_SIZE != 18)
		{
			printf("A\n");
			printf("B\n");
		}
		
		if (_vbuf && num_vertices>0)
		{
			const QRhiCommandBuffer::VertexInput vbufBinding(_vbuf, 0);
			command_buffer->setVertexInput(0, 1, &vbufBinding);
			command_buffer->draw(get_num_vertices());

#if 0
			const QRhiCommandBuffer::VertexInput bindings[] = {{_vertex_buffer, 0}};
            command_buffer->setVertexInput(0, 1, bindings);
            command_buffer->draw(_num_vertices_in_buffer);
#endif
			
			//if (VERTEX_SIZE != 18)
			//	printf("--- Rendered %d vertices. Vertex size: %d\n", get_num_vertices(), VERTEX_SIZE);
		}
	}
	
	int get_num_vertices(void) const
	{
		if (VERTEX_SIZE == 18)
			return _buffer_pos * 3 / VERTEX_SIZE;
		else
			return _buffer_pos / 8; //* 3 / VERTEX_SIZE;
	}

	const float *get_buffer(void) const
	{
		return _buffer;
	}	

	int get_num_bytes(void) const
	{
		return _buffer_pos * sizeof(float);
	}

	void addVertex(const float *vertex)
	{
		if (_buffer_pos+VERTEX_SIZE >= _buffer_size)
		{
			_buffer_size *= 2;
			_buffer = (float*)realloc(_buffer, _buffer_size * sizeof(float));
		}

		memcpy(_buffer + _buffer_pos, vertex, VERTEX_SIZE * sizeof(float));

		_buffer_pos += VERTEX_SIZE;
	}

	
	void addVertex(std::initializer_list<float> list)
	{
		//fprintf(stderr,"Size: %d\n", (int)(list.end()-list.begin()));
		assert((list.end() - list.begin()) == VERTEX_SIZE);
		addVertex(list.begin());
	}
};

#define TRIANGLE_VERTEX_SIZE 18
struct TriangleContext : public Context<TRIANGLE_VERTEX_SIZE>
{
	/*
	void addBindings(QRhi *rhi, QVector<QRhiShaderResourceBinding> &bindings) const //QRhiShaderResourceBindings *shader_resource_bindings)
	{
	}
	*/
	
	void addTriangle(float x1, float y1,
					 float x2, float y2,
					 float x3, float y3,
					 float r, float g, float b, float a,
					 float r2, float g2, float b2, float a2)
	{
		float f[TRIANGLE_VERTEX_SIZE];

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
		
		addVertex(f);
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


#define TEXTURE_VERTEX_SIZE (8*6)
struct TextureContext : public Context<TEXTURE_VERTEX_SIZE>
{
	/*
	void addBindings(QRhi *rhi, QVector<QRhiShaderResourceBinding> &bindings) const //QRhiShaderResourceBindings *shader_resource_bindings)
	{
	}
	*/
	
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
		float f[TEXTURE_VERTEX_SIZE];

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

		addVertex(f);
	}
};

}

extern r::TriangleContext *g_context_under_text; // Scrolling: Everything painted under text (only the track backgruond color.)
extern r::TriangleContext *g_context_text; // Scrolling: Text.
extern r::TriangleContext *g_context; // Scrolling: Everything painted above text)
extern r::TriangleContext *g_context_left_slider; // Left slider (Scrolls it's own way)
extern r::TriangleContext *g_context_static; // Non-Scrolling: Cursor + Indicators

//extern QVector<r::Context> g_all_contexts = {};

#define ALL_CONTEXTS {g_context_under_text, g_context_text, g_context, g_context_left_slider, g_context_static}




