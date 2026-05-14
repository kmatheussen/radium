
namespace r
{

struct Vertex
{
    float x, y;     // position
    float u, v;     // texture coordinates
    float r, g, b, a;
    
    Vertex()
		: x(0), y(0)
		, u(0), v(0)
		, r(1), g(1), b(1), a(1)
	{
	}
	
    Vertex(float _x, float _y,
		   float _u, float _v,
		   float _r, float _g, float _b, float _a)
        : x(_x), y(_y)
		, u(_u), v(_v)
		, r(_r), g(_g), b(_b), a(_a)
	{
	}
};

static float getNDC_y(float y, float screenHeight)
{
	return y;
	#if 0
	return scale(y,
				 0, screenHeight,
				 0, 1);
	#endif
	return 1.0f - 2.0f * y / screenHeight;
}

static float getNDC_x(float x, float screenWidth)
{
	return x;
	return -1.0f + 2.0f * x / screenWidth;
}

class TextureAtlas
{
public:
    // Constructor: takes QRhi, font, supported characters, and clip correction buffer
    TextureAtlas(QRhi* rhi, const QFont& font, const QString& supportedChars, QRhiBuffer* clipCorrBuffer, QRhiBuffer* scrollBuffer)
        : m_rhi(rhi)
        , m_font(font)
        , m_supportedChars(supportedChars)
        , m_clipCorrBuffer(clipCorrBuffer)
        , m_scrollBuffer(scrollBuffer)
    {
        createAtlas();
        createTextureResources();
        createShaderBindings();
    }
    
    ~TextureAtlas()
    {
        if (m_texture) {
            m_texture->destroy();
            delete m_texture;
        }
        if (m_sampler) {
            m_sampler->destroy();
            delete m_sampler;
        }
        if (m_shaderBindings) {
            m_shaderBindings->destroy();
            delete m_shaderBindings;
        }
    }
    
    // Get shader resource bindings
    QRhiShaderResourceBindings* getShaderBindings() const { return m_shaderBindings; }
    
    // Upload texture data to GPU (call once after creating atlas)
    void uploadTexture(QRhiResourceUpdateBatch* batch)
    {
        if (!m_textureUploaded && batch && !m_atlasImage.isNull()) {
            batch->uploadTexture(m_texture, m_atlasImage);
            m_textureUploaded = true;
            qDebug() << "TextureAtlas: Uploaded to GPU";
        }
    }
    
    // Append vertices for a character at position (x, y) in pixels with color
    void appendChar(std::vector<Vertex>& vertices, char c, float x, float y, float width, float height, const QColor& color = Qt::white) const
    {
        auto it = m_charUVs.find(c);
        if (it == m_charUVs.end()) {
            for (int i = 0; i < 6; ++i) {
				// Fix. No need to add vertex here?
                vertices.push_back(Vertex(0.0f, 0.0f, 0.0f, 0.0f, 
                                          color.redF(), color.greenF(), 
                                          color.blueF(), color.alphaF()));
            }
            return;
        }
        
        const UVs& uvs = it->second;

        float left = getNDC_x(x, width);
        float right = getNDC_x(x + m_charWidth, width);
        float top = getNDC_y(y, height);
        float bottom = getNDC_y(y + m_charHeight, height);
        
        float r = color.redF();
        float g = color.greenF();
        float b = color.blueF();
        float a = color.alphaF();
        
        // Triangle 1: bottom-left, bottom-right, top-right
        vertices.push_back(Vertex(left, bottom, uvs.u0, uvs.v1, r, g, b, a));
        vertices.push_back(Vertex(right, bottom, uvs.u1, uvs.v1, r, g, b, a));
        vertices.push_back(Vertex(right, top, uvs.u1, uvs.v0, r, g, b, a));
        // Triangle 2: bottom-left, top-right, top-left
        vertices.push_back(Vertex(left, bottom, uvs.u0, uvs.v1, r, g, b, a));
        vertices.push_back(Vertex(right, top, uvs.u1, uvs.v0, r, g, b, a));
        vertices.push_back(Vertex(left, top, uvs.u0, uvs.v0, r, g, b, a));
    }
    
    // Append vertices for a string with a single color
    void appendString(std::vector<Vertex>& vertices, const QString& text, 
                      float startX, float startY,
					  float width, float height,
					  const QColor& color = Qt::white) const
    {
        vertices.reserve(vertices.size() + text.length() * 6);
		
        for (int i = 0; i < text.length(); i++)
		{
            char c = text[i].toLatin1();
            float x = startX + i * m_charWidth;
            float y = startY;
            appendChar(vertices, c, x, y, width, height, color);
        }
    }
    
    // Append vertices for a string with per-character colors
    void appendStringWithColors(std::vector<Vertex>& vertices, const QString& text,
                                float startX, float startY, 
								float width, float height,
                                const std::vector<QColor>& colors) const
    {
        vertices.reserve(vertices.size() + text.length() * 6);
		
        for (int i = 0; i < text.length(); i++)
		{
            char c = text[i].toLatin1();
			startX += m_charWidth;
			if (startX > width*5)
			{
				startX = 10;
				startY += m_charHeight*0.6;
			}
            float x = startX; // + i * m_charWidth;
            float y = startY;
            QColor color = colors[i % colors.size()]; //(i < (int)colors.size()) ? colors[i] : Qt::white;
            appendChar(vertices, c, x, y, width, height, color);
        }
    }
    
    int getCharWidth() const { return m_charWidth; }
    int getCharHeight() const { return m_charHeight; }
    
private:
    struct UVs {
        float u0, v0, u1, v1;
    };
    
    void createAtlas()
    {
        if (m_supportedChars.isEmpty()) {
            qDebug() << "TextureAtlas: No supported characters provided!";
            return;
        }
        
        int charCount = m_supportedChars.length();
        m_atlasCols = static_cast<int>(std::ceil(std::sqrt(charCount)));
        m_atlasRows = (charCount + m_atlasCols - 1) / m_atlasCols;
        
        QFontMetrics fm(m_font);
        m_charWidth = fm.horizontalAdvance('W') + 4;
        m_charHeight = fm.height() + 4;
        
        int atlasWidth = m_atlasCols * m_charWidth;
        int atlasHeight = m_atlasRows * m_charHeight;
        
        m_atlasImage = QImage(atlasWidth, atlasHeight, QImage::Format_ARGB32);
        m_atlasImage.fill(Qt::transparent);
        
        QPainter painter(&m_atlasImage);
        painter.setRenderHint(QPainter::Antialiasing, true);
        painter.setRenderHint(QPainter::TextAntialiasing, true);
        painter.setFont(m_font);
        painter.setPen(Qt::white);
        
        for (int i = 0; i < charCount; ++i) {
            int col = i % m_atlasCols;
            int row = i / m_atlasCols;
            int x = col * m_charWidth + 2;
            int y = row * m_charHeight + 2;
            
            QRect rect(x, y, m_charWidth - 4, m_charHeight - 4);
            painter.drawText(rect, Qt::AlignCenter, QString(m_supportedChars[i]));
            
            float u0 = (float)(col * m_charWidth) / atlasWidth;
            float v0 = (float)(row * m_charHeight) / atlasHeight;
            float u1 = (float)((col + 1) * m_charWidth) / atlasWidth;
            float v1 = (float)((row + 1) * m_charHeight) / atlasHeight;
            
            m_charUVs[m_supportedChars[i].toLatin1()] = {u0, v0, u1, v1};
        }
        
        painter.end();
        
        qDebug() << "TextureAtlas created:" << atlasWidth << "x" << atlasHeight
                 << "with" << charCount << "characters"
                 << "(" << m_atlasCols << "x" << m_atlasRows << "grid)";
    }
    
    void createTextureResources()
    {
        if (!m_rhi) {
            qDebug() << "TextureAtlas: Cannot create texture resources - QRhi is null";
			getchar();
            return;
        }
        
        m_texture = m_rhi->newTexture(QRhiTexture::RGBA8, m_atlasImage.size(), 1, QRhiTexture::Flag{});
        if (!m_texture || !m_texture->create()) {
            qDebug() << "TextureAtlas: Failed to create texture";
			getchar();
            return;
        }
        
        m_sampler = m_rhi->newSampler(QRhiSampler::Linear, QRhiSampler::Linear,
                                      QRhiSampler::None, QRhiSampler::ClampToEdge,
                                      QRhiSampler::ClampToEdge);
        if (!m_sampler || !m_sampler->create()) {
            qDebug() << "TextureAtlas: Failed to create sampler";
			getchar();
        }
    }
    
    void createShaderBindings()
    {
        if (!m_rhi || !m_texture || !m_sampler) {
            qDebug() << "TextureAtlas: Cannot create shader bindings - resources not ready";
			getchar();
            return;
        }
        
        m_shaderBindings = m_rhi->newShaderResourceBindings();
        if (!m_shaderBindings) {
            qDebug() << "TextureAtlas: Failed to create shader resource bindings";
			getchar();
            return;
        }
        
        // Create bindings: texture at binding 0, clip correction at binding 1
        std::vector<QRhiShaderResourceBinding> bindings;
        bindings.push_back(QRhiShaderResourceBinding::sampledTexture(0,
																	 QRhiShaderResourceBinding::FragmentStage,
																	 m_texture,
																	 m_sampler));
        
        if (m_clipCorrBuffer) {
            bindings.push_back(QRhiShaderResourceBinding::uniformBuffer(1,
																		QRhiShaderResourceBinding::VertexStage,
																		m_clipCorrBuffer));
        }
        
        if (m_scrollBuffer) {
            bindings.push_back(QRhiShaderResourceBinding::uniformBuffer(2,
																		QRhiShaderResourceBinding::VertexStage,
																		m_scrollBuffer));
		}
        
        m_shaderBindings->setBindings(bindings.cbegin(), bindings.cend());
        
        if (!m_shaderBindings->create()) {
            qDebug() << "TextureAtlas: Failed to create shader resource bindings";
			getchar();
        }
    }
    
    QRhi* m_rhi = nullptr;
    QFont m_font;
    QString m_supportedChars;
    QImage m_atlasImage;
    std::map<char, UVs> m_charUVs;
    int m_charWidth = 0;
    int m_charHeight = 0;
    int m_atlasCols = 0;
    int m_atlasRows = 0;
    
    // QRhi resources owned by this class
    QRhiTexture* m_texture = nullptr;
    QRhiSampler* m_sampler = nullptr;
    QRhiShaderResourceBindings* m_shaderBindings = nullptr;
    QRhiBuffer* m_clipCorrBuffer = nullptr;
    QRhiBuffer* m_scrollBuffer = nullptr;
    bool m_textureUploaded = false;
};

} // namespace r
