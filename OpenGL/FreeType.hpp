// Based on code copied from the example program "example4.cpp" from FreeType.
//
// Original credits:
// Written Sept. 2010 by Róbert Márki <gsmiko@gmail.com>,
// with slight modifications by Werner Lemberg



#include <QImage>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include FT_TYPES_H
#include FT_OUTLINE_H
#include FT_RENDER_H

#define FT_TO_PIXEL(x) ((x) >> 6)

static FT_Library g_library;

class FreeType{
  FT_Face m_face;

public:
    
  FreeType(const char *filename, int pointSize){
    static bool is_inited = false;
    
    if (is_inited==false){
      R_ASSERT_RETURN_IF_FALSE(!FT_Init_FreeType(&g_library));

      is_inited=true;
    }
    
    
    R_ASSERT(!FT_New_Face(g_library,
                          filename,
                          0,
                          &m_face));

    R_ASSERT(!FT_Set_Char_Size(m_face,
                               0,
                               pointSize * 64,72,72//96,96
             //                               physicalDpiX(),
             //                physicalDpiY()
      ));
  }

  ~FreeType(){
    FT_Done_Face(m_face); 
  }
      
  QImage draw(char c, int width, int height){

    // 1. Create indexed image from ft.

    FT_UInt glyph_index = FT_Get_Char_Index(m_face, QChar(c).unicode());
    
    R_ASSERT(!FT_Load_Glyph(m_face,
                            glyph_index,
                            FT_LOAD_DEFAULT
                            )
             );
    

    //QImage ret_image(glyphImage.width(), glyphImage.height(), QImage::Format_ARGB32);
    QImage ret_image(width, height, QImage::Format_ARGB32);
    ret_image.fill(QColor(0.0, 0.0, 0.0, 0));
    QPainter painter(&ret_image);
    
#if 1

#if 0
    FT_Pos left = m_face->glyph->metrics.horiBearingX;
    FT_Pos right = left + m_face->glyph->metrics.width;
    FT_Pos top = m_face->glyph->metrics.horiBearingY;
    FT_Pos bottom = top - m_face->glyph->metrics.height;
    QRect m_glyphRect = QRect(QPoint(FT_TO_PIXEL(left),
                                     -FT_TO_PIXEL(top) + 1),
                              QSize(FT_TO_PIXEL(right - left) + 1,
                                    FT_TO_PIXEL(top - bottom) + 1));
#endif

    //RWarning("%d %d %d %d\n",m_glyphRect.x(),m_glyphRect.y(),m_glyphRect.width(),m_glyphRect.height());
    R_ASSERT(!FT_Render_Glyph(m_face->glyph, FT_RENDER_MODE_NORMAL));

    QImage glyphImage(m_face->glyph->bitmap.buffer,
                      m_face->glyph->bitmap.width,
                      m_face->glyph->bitmap.rows,
                      m_face->glyph->bitmap.pitch,
                      QImage::Format_Indexed8
                      );
    


    // 2. Paint the indexed image to a QImage::Format_ARGB32 image.

    //RWarning("%d %d %d %d (%d %d)\n",m_glyphRect.x(),m_glyphRect.y(),m_glyphRect.width(),m_glyphRect.height(), glyphImage.width(), glyphImage.height());

    //    if (glyphImage.width()==0 || glyphImage.height()==0)
    //   return ret_image;
      
    //    painter.translate(m_glyphRect.x(),
    //                  m_glyphRect.y());
    
    QVector<QRgb> colorTable;
    for (int i = 0; i < 256; ++i)
      colorTable << qRgba(0, 0, 0, i);
    glyphImage.setColorTable(colorTable);
    
    painter.drawImage(QPoint(0, 0),
                      glyphImage);

#else
    ret_image.fill(QColor(0.0, 0.0, 0.0, 0));
    QRect rect(0,0,width,height);
    painter.drawText(rect, Qt::AlignVCenter, QString(QChar(c)));
#endif
    
    return ret_image; //.convertToFormat(QImage::Format_ARGB32);
  }
};
