#pragma once

#include "Widget_proc.h"

namespace r
{

	
static const QString g_supportedChars = QStringLiteral("abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVW #-,.(){}<>=*:0123456789|/");


class TextureAtlasBackend
{
	friend class TextureAtlas;
	
	struct UVs
	{
        float u0, v0, u1, v1;
    };

	QRhi* _rhi = nullptr;

	mutable radium::RWLock _newImageLock;

	struct D
	{
		QImage _image;
		int _num_columns = 0;
		int _num_rows = 0;
		
		int _char_width = 0;
		int _char_height = 0;
		int _logical_char_width = 0;
		
		QMap<char, UVs> _char_uvs;
	} _d, _next_d;
	
    // QRhi resources owned by this class
    QRhiTexture* _texture = nullptr;
    QRhiSampler* _sampler = nullptr;

//    bool _textureUploaded = false;

public:

    TextureAtlasBackend(QRhi* rhi, const QFont& font)
        : _rhi(rhi)
    {
        createAtlas(font);
        QRHI_createTextureResources(_next_d._image);
    }
	
	~TextureAtlasBackend()
    {
        if (_texture)
		{
            _texture->destroy();
            delete _texture;
        }
        if (_sampler)
		{
            _sampler->destroy();
            delete _sampler;
        }
    }

	void MAIN_appendStringToVertices(r::TextureVertices *vertices,
									 const QString& text, 
									 float startX, float startY,
									 float r, float g, float b, float a)
	//					  const QColor& color = Qt::white)
		const
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		/*
        float r = color.redF();
        float g = color.greenF();
        float b = color.blueF();
        float a = color.alphaF();
        */

		radium::ScopedReadLock lock(_newImageLock);

		const D &d = _d; //_next_d._image.isNull() ? _d : _next_d;

		const double scale_ratio = g_opengl_scale_ratio;
		const float char_width_logical = d._logical_char_width;
		const float char_height_logical = d._char_height / scale_ratio;

        for (int i = 0; i < text.length(); i++)
		{
            char c = text[i].toLatin1();

			if (c==' ')
				continue;
			
			auto it = d._char_uvs.find(c);
			
			if (it == d._char_uvs.end())
			{
				printf("===Can't render this character: '%c'\n", c);
				continue;
			}
        
			const UVs& uvs = it.value();

            float x = floor(startX + i * char_width_logical) + 1;
            float y = floor(startY) + 1;
			
            vertices->MAIN_addTexture(x, y,
									  x + char_width_logical, y + char_height_logical,
									  uvs.u0, uvs.v0,
									  uvs.u1, uvs.v1,
									  r,g,b,a);
        }
	}

    // Upload texture data to GPU (call once after creating atlas)
    void QRHI_uploadTexture(QRhiResourceUpdateBatch* batch)
    {
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		if (batch==NULL)
			return;

		radium::ScopedWriteLock lock(_newImageLock);

        if (!_d._image.isNull())
		{
			if (_texture->pixelSize() != _d._image.size())
			{
				_texture->setPixelSize(_d._image.size());
				
				if (!_texture->create())
				{
					GFX_Message(NULL, "Failed to recreate texture");
				}
			}
			
            batch->uploadTexture(_texture, _d._image);

			_next_d._image = QImage();
			_d._image = QImage();
			
            //qDebug() << "TextureAtlas: Uploaded to GPU";
        }
    }

	// I suspected that switching at the wrong time could cause flickering, but all this might be (or is probably) just unnecessary complication.
	//
	// This function is called right before starting a new render, on the same thread that is rendering (which is currently the main thread).
	//
	void MAIN_maybe_switch_to_next_d(void)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		radium::ScopedWriteLock lock(_newImageLock);

		if (!_next_d._image.isNull())
		{
			_d = std::move(_next_d);
			_next_d._image = QImage();
		}
	}
	
	void MAIN_setFont(const QFont &nonscaled_font)
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		createAtlas(nonscaled_font);
	}
	
    int QRHI_getFontHeight(void) const
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

    	radium::ScopedReadLock lock(_newImageLock);
    	return _d._char_height / g_opengl_scale_ratio;
    }
    
private:

	// Note: Can be called from both the main thread and the qrhi thread.
    void createAtlas(const QFont &orgfont)
    {
        int charCount = g_supportedChars.length();

		D d;
		
        d._num_columns = static_cast<int>(std::ceil(std::sqrt(charCount)));
        d._num_rows = (charCount + d._num_columns - 1) / d._num_columns;

		{
			QFont logical_font(orgfont);
			logical_font.setHintingPreference(QFont::PreferFullHinting);
			QFontMetrics logical_metrics(logical_font);
			d._logical_char_width = logical_metrics.horizontalAdvance("#");
		}

#if 0
        QFontMetrics fm(_font);
        int char_width = fm.horizontalAdvance('W') + 4;
        int char_height = fm.height() + 4;
#endif

		const double scale_ratio = g_opengl_scale_ratio;
		
		QFont font(orgfont);
		if(!equal_doubles(scale_ratio, 1.0))
		{
			QFontMetrics fm_unscaled(orgfont);
			font.setPixelSize(fm_unscaled.height() * scale_ratio);
		}
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
            painter.drawText(rect, Qt::AlignBottom, QString(g_supportedChars[i]));
            
            float u0 = (float)(x + 0.5f) / atlasWidth;
            float v0 = (float)(y + 0.5f) / atlasHeight;
            float u1 = (float)(x + d._char_width + 0.5f) / atlasWidth;
            float v1 = (float)(y + d._char_height + 0.5f) / atlasHeight;
            
            d._char_uvs[g_supportedChars[i].toLatin1()] = {u0, v0, u1, v1};
        }
        
        painter.end();

		/*
        qDebug() << "TextureAtlas created:" << atlasWidth << "x" << atlasHeight
                 << "with" << charCount << "characters"
                 << "(" << d._num_columns << "x" << d._num_rows << "grid)";
		*/
		
		{
			radium::ScopedWriteLock lock(_newImageLock);

			_next_d = d;
		}
    }
    
    void QRHI_createTextureResources(const QImage &image)
    {
        R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

        if (!_rhi)
		{
            GFX_Message(NULL, "TextureAtlas: Cannot create texture resources - QRhi is null");
            return;
        }
        
        _texture = _rhi->newTexture(QRhiTexture::RGBA8,
									  image.size(),
									  1,
									  QRhiTexture::Flag{});
		
        if (!_texture || !_texture->create())
		{
            GFX_Message(NULL, "TextureAtlas: Failed to create texture");
            return;
        }

		if (GL_get_clamp_text_rendering())
		{
			////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
			/////////// ALWAYS TEST THIS BRANCH IF CHANGING SOMETHING YOU SUSPECT COULD CHANGE TEXT-SCALING ////////////////////
			////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
			// This one ensures non-scaled text (no bluring), but also causes jumpy text when scrolling.
			_sampler = _rhi->newSampler(QRhiSampler::Nearest,
			                            QRhiSampler::Nearest,
			                            QRhiSampler::None,
			                            QRhiSampler::ClampToEdge,
			                            QRhiSampler::ClampToEdge);
		}
		else
		{
			// This one doesn't guarantee non-scaled text, but we should not experience it anyway (since the parameters we're using shouldn't cause scaling).
			_sampler = _rhi->newSampler(QRhiSampler::Linear,
			                            QRhiSampler::Linear,
			                            QRhiSampler::None,
			                            QRhiSampler::ClampToEdge,
			                            QRhiSampler::ClampToEdge);
		}
		
        if (!_sampler || !_sampler->create())
		{
			GFX_Message(NULL, "TextureAtlas: Failed to create sampler");
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
        QRHI_createShaderBindings();
    }
    
    ~TextureAtlas()
    {
        if (_shaderBindings)
        {
            _shaderBindings->destroy();
            delete _shaderBindings;
        }
    }
    
	QRhiShaderResourceBindings* QRHI_getShaderBindings(void) const
	{
		R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

		return _shaderBindings;
	}
    
    void MAIN_appendStringToVertices(r::TextureVertices *vertices,
									 const QString& text, 
									 float startX, float startY,
									 float r, float g, float b, float a)
		const
	{
		R_ASSERT_NON_RELEASE(THREADING_is_main_thread());

		_backend->MAIN_appendStringToVertices(vertices,
											  text,
											  startX, startY,
											  r, g, b, a);
	}
	
    void QRHI_createShaderBindings(void)
    {
        R_ASSERT_NON_RELEASE(THREADING_is_qrhi_thread());

        if (!_rhi || !_backend->_texture || !_backend->_sampler)
		{
            GFX_Message(NULL, "TextureAtlas: Cannot create shader bindings - resources not ready");
            return;
        }
        
        _shaderBindings = _rhi->newShaderResourceBindings();
		
        if (!_shaderBindings)
		{
            GFX_Message(NULL, "TextureAtlas: Failed to create shader resource bindings");
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
            GFX_Message(NULL, "TextureAtlas: Failed to create shader resource bindings");
        }
    }
};

} // namespace r
