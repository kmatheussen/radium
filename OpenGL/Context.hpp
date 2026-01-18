
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#  include <rhi/qrhi.h>
#pragma GCC diagnostic pop


namespace r
{
struct Context
{
	static constexpr int START_SIZE = 200; // Number of triangles
	static constexpr int TRIANGLE_SIZE = 18; // Number of floats in one triangle (pos1 + pos1-color, pos2 + pos2-color, pos3 + pos3-color.)

	int _buffer_size = TRIANGLE_SIZE * 200;
	int _buffer_pos = 0;
	float *_buffer = (float*)malloc(_buffer_size * sizeof(float));

	QRhiBuffer *_vbuf;

	QRhiResourceUpdateBatch *_updates = nullptr;
	
	Context()
	{	
	}

	~Context()
	{
		delete _vbuf;
		free(_buffer);
	}

	void call_me_when_finished_painting(QRhi *rhi)
	{
		static int total = 0;
		total += get_num_bytes();
		printf("Num bytes: %d. Total: %d\n", get_num_bytes(), total);
		
		_vbuf = rhi->newBuffer(QRhiBuffer::Static, // Note: Possible optimization here. Don't really understand difference between Static, Immutable, and Dynamic.
							   QRhiBuffer::VertexBuffer,
							   get_num_bytes());

		_vbuf->create();
	
		_updates = rhi->nextResourceUpdateBatch();
		_updates->uploadStaticBuffer(_vbuf, get_buffer());
	}
	
	void maybe_merge_in(QRhiResourceUpdateBatch *update_batch)
	{
		if (_updates)
		{
			printf("MERGING UPDATES\n");
			update_batch->merge(_updates);
			_updates->release();
			_updates = nullptr;
		}
	}

	void render(QRhiCommandBuffer *cb)
	{
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
		_vbuf->deleteLater();
		_vbuf = NULL;
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
					 float r, float g, float b)
	{
		float a = 0.01;
		
		float f[TRIANGLE_SIZE];
		f[0] = scale(x1,
					 0, 1000,
					 -1, 1);
		f[1] = scale(y1,
					 0, 4000,
					 -1, 1);
		f[2] = r; f[3] = g; f[4] = b; f[5] = a;
		
		f[6] = scale(x2,
					 0, 1000,
					 -1, 1);
		f[7] = scale(y2,
					 0, 4000,
					 -1, 1);
		f[8] = r; f[9] = g; f[10] = b; f[11] = a;
		
		f[12] = scale(x3,
					 0, 1000,
					 -1, 1);
		f[13] = scale(y3,
					 0, 4000,
					 -1, 1);

		f[14] = r; f[15] = g; f[16] = b; f[17] = a;

		addTriangle(f);
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
