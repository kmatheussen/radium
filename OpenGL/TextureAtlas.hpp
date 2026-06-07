
namespace r
{

	
static const QString g_supportedChars = QStringLiteral("abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVW #-,.(){}<>=*:0123456789");


class TextureAtlasBackend
{
	friend class TextureAtlas;
	
	struct UVs
	{
        float u0, v0, u1, v1;
    };

	QRhi* _rhi = nullptr;

	mutable radium::Mutex _newImageLock;

	struct D
	{
		QImage _image;
		int _num_columns = 0;
		int _num_rows = 0;
		
		int _char_width = 0;
		int _char_height = 0;
		
		QMap<char, UVs> _char_uvs;
	} _d, _next_d;
	
    // QRhi resources owned by this class
    QRhiTexture* _texture = nullptr;
    QRhiSampler* _sampler = nullptr;

    bool _textureUploaded = false;

public:

    TextureAtlasBackend(QRhi* rhi, const QFont& font)
        : _rhi(rhi)
    {
        createAtlas(font);
        createTextureResources(_next_d._image);
    }
	
	~TextureAtlasBackend()
    {
        if (_texture) {
            _texture->destroy();
            delete _texture;
        }
        if (_sampler) {
            _sampler->destroy();
            delete _sampler;
        }
    }

	void appendStringToVertices(r::TextureVertices *vertices,
								const QString& text, 
								float startX, float startY,
								float r, float g, float b, float a)
		//					  const QColor& color = Qt::white)
		const
	{
		/*
        float r = color.redF();
        float g = color.greenF();
        float b = color.blueF();
        float a = color.alphaF();
        */

		radium::ScopedMutex lock(_newImageLock);

		const D &d = _d; //_next_d._image.isNull() ? _d : _next_d;

        for (int i = 0; i < text.length(); i++)
		{
            char c = text[i].toLatin1();

			if (c==' ')
				continue;
			
			auto it = d._char_uvs.find(c);
			
			if (it == d._char_uvs.end()) {
				printf("===Can't render this character: '%c'\n", c);
				continue;
			}
        
			const UVs& uvs = it.value();

            float x = floor(startX + i * d._char_width) + 1;
            float y = floor(startY) + 1;
			
            vertices->addTexture(x, y,
								 x + d._char_width, y + d._char_height,
								 uvs.u0, uvs.v0,
								 uvs.u1, uvs.v1,
								 r,g,b,a);
        }
	}

    // Upload texture data to GPU (call once after creating atlas)
    void uploadTexture(QRhiResourceUpdateBatch* batch)
    {
		if (batch==NULL)
			return;

		radium::ScopedMutex lock(_newImageLock);

        if (!_d._image.isNull())
		{
			if (_texture->pixelSize() != _d._image.size())
			{
				_texture->setPixelSize(_d._image.size());
				
				if (!_texture->create()) {
					qFatal("Failed to recreate texture");
				}
			}
			
            batch->uploadTexture(_texture, _d._image);

			_next_d._image = QImage();
			_d._image = QImage();
			
            qDebug() << "TextureAtlas: Uploaded to GPU";
        }
    }

	// Suspected switching at the wrong time could cause flickering, but all this is probably just unnecessary complication.
	// (the flickering is caused by something else)
	//
	// This function is called right before starting a new render, on the same thread that is rendering (which is currently the main thread).
	//
	void maybe_switch_to_next_d(void)
	{
		radium::ScopedMutex lock(_newImageLock);

		if (!_next_d._image.isNull())
		{
			_d = std::move(_next_d);
			_next_d._image = QImage();
		}
	}
	
	void setFont(const QFont &nonscaled_font)
	{
#if 1
		createAtlas(nonscaled_font);
#else
		QFont font(nonscaled_font);

		const double scale_ratio = safe_double_read(&g_opengl_scale_ratio);
		
		if(!equal_doubles(scale_ratio, 1.0))
			font.setPointSize(font.pointSize() * scale_ratio);

		createAtlas(font);
#endif
	}
	
    //int getCharWidth() const { return _char_width; }
    //int getCharHeight() const { return _char_height; }
    
private:
    
    void createAtlas(const QFont &orgfont)
    {
        int charCount = g_supportedChars.length();

		D d;
		
        d._num_columns = static_cast<int>(std::ceil(std::sqrt(charCount)));
        d._num_rows = (charCount + d._num_columns - 1) / d._num_columns;

#if 0
        QFontMetrics fm(_font);
        int char_width = fm.horizontalAdvance('W') + 4;
        int char_height = fm.height() + 4;
#endif

		QFont font(orgfont);
		font.setHintingPreference(QFont::PreferFullHinting); // full hinting should look better

		QFontMetrics metrics(font);
		
		int real_width = metrics.horizontalAdvance("#"); //(void)real_width;
		d._char_height = metrics.height();
		d._char_width = real_width; //char_height;

        int atlasWidth = d._num_columns * d._char_width;
        int atlasHeight = d._num_rows * d._char_height;
        
        d._image = QImage(atlasWidth, atlasHeight, QImage::Format_ARGB32);

        //image.fill(Qt::transparent);
        
        QPainter painter(&d._image);
#if 0
        painter.setRenderHint(QPainter::Antialiasing, true);
        painter.setRenderHint(QPainter::TextAntialiasing, true);
        painter.setFont(font);
        painter.setPen(Qt::white);
#endif

		painter.setPen(QColor(255, 255, 255, 255));

		painter.setFont(font);

		QColor qcol("#fefefe");
		//QColor qcol("#c0c0c0c0");
		qcol.setAlpha(0);
		d._image.fill(qcol);//QColor(0.0, 0.0, 0.0, 0));

        for (int i = 0; i < charCount; ++i)
		{			
            int col = i % d._num_columns;
            int row = i / d._num_columns;
            int x = col * d._char_width;
            int y = row * d._char_height;
            
            QRect rect(x, y, d._char_width, d._char_height);
            painter.drawText(rect, Qt::AlignVCenter, QString(g_supportedChars[i]));
            
            float u0 = (float)(x + 0.5f) / atlasWidth;
            float v0 = (float)(y + 0.5f) / atlasHeight;
            float u1 = (float)(x + d._char_width + 0.5f) / atlasWidth;
            float v1 = (float)(y + d._char_height + 0.5f) / atlasHeight;
            
            d._char_uvs[g_supportedChars[i].toLatin1()] = {u0, v0, u1, v1};
        }
        
        painter.end();
        
        qDebug() << "TextureAtlas created:" << atlasWidth << "x" << atlasHeight
                 << "with" << charCount << "characters"
                 << "(" << d._num_columns << "x" << d._num_rows << "grid)";

		{
			radium::ScopedMutex lock(_newImageLock);

			_next_d = d;
		}
    }
    
    void createTextureResources(const QImage &image)
    {
        if (!_rhi) {
            qDebug() << "TextureAtlas: Cannot create texture resources - QRhi is null";
			getchar();
            return;
        }
        
        _texture = _rhi->newTexture(QRhiTexture::RGBA8,
									  image.size(),
									  1,
									  QRhiTexture::Flag{});
		
        if (!_texture || !_texture->create()) {
            qDebug() << "TextureAtlas: Failed to create texture";
			getchar();
            return;
        }

#if 0
		// This one ensures non-scaled text (no bluring), but also causes jumpy text when scrolling.
		// (Try to enable it to see if text becomes clearer. If it does, something is wrong.)
        _sampler = _rhi->newSampler(QRhiSampler::Nearest,
									QRhiSampler::Nearest,
									QRhiSampler::None,
									QRhiSampler::ClampToEdge,
									QRhiSampler::ClampToEdge);
#else
		// This one doesn't guarantee non-scaled text, but we should not experience it anyway (since the parameters we're using shouldn't cause scaling).
		// We also avoid jumpy text when scrolling using this one. */
        _sampler = _rhi->newSampler(QRhiSampler::Linear,
									QRhiSampler::Linear,
									QRhiSampler::None,
									QRhiSampler::ClampToEdge,
									QRhiSampler::ClampToEdge);
#endif
		
        if (!_sampler || !_sampler->create()) {
            qDebug() << "TextureAtlas: Failed to create sampler";
			getchar();
        }
    }
    
};
	
class TextureAtlas
{
	QRhi *_rhi;
	
	TextureAtlasBackend *_backend;
	
    QRhiShaderResourceBindings* _shaderBindings = nullptr;
    QRhiBuffer* _viewCorrectionBuffer = nullptr;
    QRhiBuffer* _scrollPosBuffer = nullptr;

public:
	
    TextureAtlas(QRhi* rhi, TextureAtlasBackend *backend, QRhiBuffer* viewCorrectionBuffer, QRhiBuffer* scrollPosBuffer)
        : _rhi(rhi)
		, _backend(backend)
        , _viewCorrectionBuffer(viewCorrectionBuffer)
        , _scrollPosBuffer(scrollPosBuffer)
    {
        createShaderBindings();
    }
    
    ~TextureAtlas()
    {
    }
    
    // Get shader resource bindings
	QRhiShaderResourceBindings* getShaderBindings(void) const
	{
		return _shaderBindings;
	}
    
    void appendStringToVertices(r::TextureVertices *vertices,
								const QString& text, 
								float startX, float startY,
								float r, float g, float b, float a)
		const
	{
		_backend->appendStringToVertices(vertices,
							   text,
							   startX, startY,
							   r, g, b, a);
	}
	
    void createShaderBindings(void)
    {
        if (!_rhi || !_backend->_texture || !_backend->_sampler) {
            qDebug() << "TextureAtlas: Cannot create shader bindings - resources not ready";
			getchar();
            return;
        }
        
        _shaderBindings = _rhi->newShaderResourceBindings();
        if (!_shaderBindings)
		{
            qDebug() << "TextureAtlas: Failed to create shader resource bindings";
			getchar();
            return;
        }
        
        // Create bindings: texture at binding 0, view correction at binding 1, scroll-pos at binding 2
        std::vector<QRhiShaderResourceBinding> bindings;
		
        bindings.push_back(QRhiShaderResourceBinding::sampledTexture(0,
																	 QRhiShaderResourceBinding::FragmentStage,
																	 _backend->_texture,
																	 _backend->_sampler));
        
        if (_viewCorrectionBuffer)
		{
            bindings.push_back(QRhiShaderResourceBinding::uniformBuffer(1,
																		QRhiShaderResourceBinding::VertexStage,
																		_viewCorrectionBuffer));
        }
        
        if (_scrollPosBuffer)
		{
            bindings.push_back(QRhiShaderResourceBinding::uniformBuffer(2,
																		QRhiShaderResourceBinding::VertexStage,
																		_scrollPosBuffer));
		}
        
        _shaderBindings->setBindings(bindings.cbegin(), bindings.cend());
        
        if (!_shaderBindings->create())
		{
            qDebug() << "TextureAtlas: Failed to create shader resource bindings";
			getchar();
        }
    }
};

} // namespace r
