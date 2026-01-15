/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include <vlCore/Say.hpp>
#include <cmath>

using namespace vl;

SayArg::SayArg()
{
  init();
}

SayArg::SayArg(void* d)
{
  init();
  ulonglong = reinterpret_cast<unsigned long long>(d);
  type = ULONGLONG;
}

SayArg::SayArg(const std::string& d)
{
  init();
  str = d.c_str();
  type = STRING;
}

SayArg::SayArg(const unsigned char* d)
{
  init();
  if (d)
    str = (const char*)d;
  type = STRING;
}

SayArg::SayArg(const char* d)
{
  init();
  if (d)
    str = d;
  type = STRING;
}

SayArg::SayArg(const String& d)
{
  init();
  str = d;
  type = STRING;
}

SayArg::SayArg(double d)
{
  init();
  float64 = d;
  type = FLOAT64;
}

SayArg::SayArg(float d)
{
  init();
  float64 = d;
  type = FLOAT64;
}

SayArg::SayArg(unsigned char d)
{
  init();
  ulonglong = d;
  type = ULONGLONG;
}

SayArg::SayArg(signed char d)
{
  init();
  slonglong = d;
  type = SLONGLONG;
}

SayArg::SayArg(unsigned short d)
{
  init();
  ulonglong = d;
  type = ULONGLONG;
}

SayArg::SayArg(signed short d)
{
  init();
  slonglong = d;
  type = SLONGLONG;
}

SayArg::SayArg(unsigned int d)
{
  init();
  ulonglong = d;
  type = ULONGLONG;
}

SayArg::SayArg(signed int d)
{
  init();
  slonglong = d;
  type = SLONGLONG;
}

SayArg::SayArg(unsigned long d)
{
  init();
  ulonglong = d;
  type = ULONGLONG;
}

SayArg::SayArg(signed long d)
{
  init();
  slonglong = d;
  type = SLONGLONG;
}

SayArg::SayArg(unsigned long long d)
{
  init();
  ulonglong = d;
  type = ULONGLONG;
}

SayArg::SayArg(signed long long d)
{
  init();
  slonglong = d;
  type = SLONGLONG;
}

void SayArg::init()
{
  type = NO_TYPE;
  float64 = 0;
  ulonglong = 0;
  slonglong = 0;
}

String Say::parse( const Say& pset ) const
{
  String out;
  String fmt = pset.format_string;

  int param_idx = 0;

  int eur = -1;
  int base = -1;
  int field = -1;
  int decimals = -1;
  int align = -1; 
  int fill = -1;
  int plus = -1;
  
  int fmtstart = -1;

  // %H+014.5n
  bool fmtdata = false;
  for(int i=0; i<(int)fmt.length(); ++i)
  {
    int ch       = (i<(int)fmt.length())   ? (int)fmt[i+0] : -1;
    int next_ch  = (i<(int)fmt.length()-1) ? (int)fmt[i+1] : -1;
    int nnext_ch = (i<(int)fmt.length()-2) ? (int)fmt[i+2] : -1;

    if (!fmtdata)
    {
      if (ch == '%' && next_ch == '%')
      {
        out += '%';
        ++i;
      }
      else
      if (ch == '%')
      {
        if (param_idx < (int)pset.size())
        {
          fmtdata = true;
          fmtstart = i;
        }
        else
        {
          out += " !!!too few parameters: %";
        }
      }
      else
      if (ch >= 0)
      {
        out += (unsigned short)ch;
      }
    }
    else
    {

      if(eur == -1)
      {
        if (ch == '$')
        {
          eur = 1;
          continue;
        }
      }

      if (base == -1)
      {
        switch(ch)
        {
        case 'b': base = 2;  break;
        case 'o': base = 8;  break;
        case 'd': base = 10; break;
        case 'h': base = 16; break;
        }
        if (base != -1)
        {
          if (eur == -1)
            eur = 0;
          continue;
        }
      }

      if (plus == -1)
      {
        switch(ch)
        {
        case '+': plus = 1; break;
        }
        if (plus != -1)
        {
          if (base == -1)
            base = 10;
          if (eur == -1)
            eur = 0;
          continue;
        }
      }

      if (fill == -1)
      {
        switch(ch)
        {
        case '0': fill = '0'; break;
        case ' ': fill = ' '; break;
        }
        if (fill != -1)
        {
          if (base == -1)
            base = 10;
          if (plus == -1)
            plus = 0;
          if (eur == -1)
            eur = 0;
          continue;
        }
      }

      if (field == -1)
      {
        if (ch >= '0' && ch <= '9')
        {
          field = ch - '0';
          if (next_ch >= '0' && next_ch <= '9')
          {
            field = field*10 + next_ch - '0';
            ++i;
          }
        }

        if (field != -1)
        {
          if (base == -1)
            base = 10;
          if (plus == -1)
            plus = 0;
          if (fill == -1)
            fill = ' ';
          if (eur == -1)
            eur = 0;
          continue;
        }
      }

      if (decimals == -1)
      {
        if(ch == '.')
        {
          if (next_ch >= '0' && next_ch <= '9')
          {
            decimals = next_ch - '0';
            ++i;
            if (nnext_ch >= '0' && nnext_ch <= '9')
            {
              decimals = decimals*10 + nnext_ch - '0';
              ++i;
            }
          }
        }

        if (decimals != -1)
        {
          if (base == -1)
            base = 10;
          if (plus == -1)
            plus = 0;
          if (fill == -1)
            fill = ' ';
          if (field == -1)
            field = 0;
          if (eur == -1)
            eur = 0;
          continue;
        }
      }

      if (align == -1)
      {
        if(ch == '=')
          align = 0;
        if(ch == '<')
          align = 1;
        if(ch == '>')
          align = 2;

        if (align != -1)
        {
          if (base == -1)
            base = 10;
          if (plus == -1)
            plus = 0;
          if (fill == -1)
            fill = ' ';
          if (field == -1)
            field = 0;
          if (eur == -1)
            eur = 0;
          if (decimals == -1)
          {
            switch(pset[param_idx].type)
            {
            case SayArg::FLOAT64: decimals = 6; break;
            default: decimals = 0; break;
            }
          }
          continue;
        }
      }

      // generate formatted string

      // output parameter
      const SayArg& p = pset[param_idx];

      if (ch == 'c')
      {
        if (fmtstart != i-1)
          out += " !!! '%c' does not need arguments !!! ";

        switch(p.type)
        {
        case SayArg::FLOAT64: out += (char)p.float64; break;
        case SayArg::SLONGLONG: out += (char)p.slonglong; break;
        case SayArg::ULONGLONG: out += (char)p.ulonglong; break;
        default:
          out += " !!! wrong argument type for '%c' !!! ";
          break;
        }

      }
      else
      if (ch == 's')
      {
        if (fmtstart != i-1)
          out += " !!! '%s' does not need arguments !!! ";

        switch(p.type)
        {
        case SayArg::STRING: out += p.str; break;
        default:
          out += " !!! wrong argument type for '%s' !!! ";
          break;
        }

      }
      else
      if (ch == 'n' || ch == 'N' || ch == 'e' || ch == 'E')
      {

        if (param_idx<(int)pset.size())
        {
          if (decimals == -1)
          {
            switch(p.type)
            {
            case SayArg::FLOAT64: decimals = 6; break;
            default: decimals = 0; break;
            }
          }

          if (base == -1)
            base = 10;
          if (field == -1)
            field = 0;
          if (decimals == -1)
            decimals = 0;
          if (fill == -1)
            fill = ' ';
          if (plus == -1)
            plus = 0;
          if (align == -1)
            align = 2;
          if (eur == -1)
            eur = 0;

          switch(p.type)
          {
          case SayArg::FLOAT64:   out += format(p.float64, base, field, decimals, align, fill, plus, ch, eur); break;
          case SayArg::SLONGLONG: out += format(p.slonglong, base, field, decimals, align, fill, plus, ch, eur); break;
          case SayArg::ULONGLONG: out += format(p.ulonglong, base, field, decimals, align, fill, plus, ch, eur); break;
          default: 
            out += " !!! wrong argument type for '%n' !!! ";
            break;
          }
        }
        else
        {
          out += " !!!missing parameter!!! ";
          if (ch != -1)
            i--;
        }
      }
      else
      {
        out += " !!!format error: unexpected '";
        out += (char)ch;
        out += "' !!! ";
      }

      fmtdata = false;
      align = -1;
      base = -1;
      field = -1;
      decimals = -1;
      align = -1; 
      fill = -1;
      plus = -1;
      eur = -1;

      param_idx++;
    }
  }

  if (fmtdata)
  {
    out += " !!!truncated format!!! ";
    param_idx++;
  }

  if (param_idx < (int)pset.size())
    out += " !!!too many parameters!!! ";

  return out;
  // ... fare in modo che l'output venga generato anche quando non c'e' il carattere finale ...
}

String Say::euronotation(const String& str, int base) const
{
  String tmp;
  int pos = (int)str.length();
  if ( str.contains('.') )
  {
    while(pos--)
    {
      if (str[pos] == '.')
      {
        tmp.insert(0, ',');
        break;
      }
      tmp.insert(0, str[pos]);
    }
    if (pos < 0)
      pos = (int)str.length();
  }

  int count = 0;
  int wait = 3;
  if (base == 2)
    wait = 4;
  if (base == 16)
    wait = 2;
  while(pos--)
  {
    if (count && count % wait == 0)
    {
      tmp.insert(0, '.');
    }
    tmp.insert(0, str[pos]);
    count ++;
  }

  return tmp;
}
  
String Say::format(unsigned long long n, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const
{
  if (field < 0)
    field = -field;

  if (field > 1024)
    field = 1024;

  if (decimals < 0)
    decimals = -decimals;
  if (decimals > 20)
    decimals = 20;

  if (align != 0 && align != 1 && align != 2)
    align = 0;

  if (base > 16)
    base = 16;

  if (base < 2)
    base = 2;

  String str;

  const char* hex = "0123456789abcdef";

  // UNSIGNED INT ALGORITHM

  int k = base;
  do
  {
    int x = (int)(n % base);
    int c = x/(k/base);
    str.insert(0, hex[c]);
    n = n  / base;
  }
  while(n);

  if (decimals)
  {
    str += '.';
    int i = decimals;
    while(i--)
      str += '0';
  }

  bool negative = false;

  return pipeline(str, base, field, decimals, finalizer, align, eur, fill, negative, plus); 
}

String Say::format(signed long long nn, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const
{
  if (field < 0)
    field = -field;

  if (field > 1024)
    field = 1024;

  if (decimals < 0)
    decimals = -decimals;
  if (decimals > 20)
    decimals = 20;

  if (align != 0 && align != 1 && align != 2)
    align = 0;

  if (base > 16)
    base = 16;

  if (base < 2)
    base = 2;

  String str;

  const char* hex = "0123456789abcdef";

  // SIGNED INT ALGORITHM

  bool negative = nn < 0;
  unsigned long long n;

  if (nn<0 && -nn<0) // overflow
    n = (unsigned long long)nn;
  else
  if (nn<0)
    n = - nn;
  else
    n = nn;

  //if (n < 0)
  // n = 0;

  int k = base;
  do
  {
    int x = (int)(n % base);
    int c = x/(k/base);
    str.insert(0, hex[c]);
    n = n  / base;
  }
  while(n);
  
  if (decimals)
  {
    str += '.';
    int i = decimals;
    while(i--)
      str += '0';
  }

  return pipeline(str, base, field, decimals, finalizer, align, eur, fill, negative, plus); 
}
  
String Say::format(double num, int base, int field, int decimals, int align, int fill, int plus, int finalizer, int eur) const
{
  if (field < 0)
    field = -field;
  if (field > 1024)
    field = 1024;

  if (decimals < 0)
    decimals = -decimals;
  if (decimals > 20)
    decimals = 20;

  if (align != 0 && align != 1 && align != 2)
    align = 0;

  if (base > 16)
    base = 16;

  if (base < 2)
    base = 2;

  String str;

  const char* hex = "0123456789abcdef";

  double f = num;

  // INDEFINITE = - 127 192 0 0
  // -INFINITE  = - 127 128 0 0
  // +INFINITE  = + 127 128 0 0 
  float tmp = (float)f;
  unsigned char *nan= (unsigned char*)&tmp;
  const char* sign = nan[3] >= 128 ? "-" : "+";
  unsigned char exp = (nan[3] << 1) + (nan[2] >> 7);
  nan[2] &= 127;
  unsigned int frac = nan[0] + (nan[1] << 8) + (nan[2] << 16);

  bool negative = false;
  if (exp == 255 && frac == 0)
  {
    return String(sign) + "#INF";
  }
  else
  if (exp == 255 && frac != 0)
  {
    return "#NAN";
  }
  else
  {
    // ROUNDING FOR FRACTIONAL PART

    if (finalizer == 'n' || finalizer == 'N')
    {
      double fp = f - floor(f); 
      double eps = base/2;
      int dec = decimals;
      do
      {
        if ( !(dec--) )
          break;

        int c = (int)(fp * base);
        fp = fp * base - c;

        eps /= base;

        if (c<0 || c>15)
        {
          return "#ERR";
        }

        if (dec == 0) // round only if all the decimals are here
        {
          // program rounded fp
          f += eps/base;
          break;
        }
      }
      while(fp>0);
    }
      
    if (f < 0)
    {
      f = -f;
      negative = true;
    }
    double n = floor(f);

    // INTEGER PART

    int count = 0; 
    unsigned int base2 = base*base;
    unsigned int base3 = base*base*base;
    unsigned int base4 = base*base*base*base;
    unsigned int base5 = base*base*base*base*base;
    unsigned int base6 = base*base*base*base*base*base;
    unsigned int base7 = base*base*base*base*base*base*base; // maximum number in base 16
    while (floor(n))
    {
      if (n>=base7)
      {
        n /= base7;
        count+=7;
      }
      else
      if (n>=base6)
      {
        n /= base6;
        count+=6;
      }
      else
      if (n>=base5)
      {
        n /= base5;
        count+=5;
      }
      else
      if (n>=base4)
      {
        n /= base4;
        count+=4;
      }
      else
      if (n>=base3)
      {
        n /= base3;
        count+=3;
      }
      else
      if (n>=base2)
      {
        n /= base2;
        count+=2;
      }
      else
      {
        n = n / base;
        count++;
      }
    }

    // prevents rounding errors
    double eps = (base / 2.0) / base;
    for(int i=0; i<count; ++i)
    {
      eps /= base;
    }
    n+=eps;

    if (count)
    {
      do
      {
        int c = (int)(n * (double)base);
        n = n * (double)base - floor(n * (double)base);
        int next = (int)(n * base);

        if (c<0 || c>15 || next<0 || next>15)
        {
          return "#ERR";
        }

        str += hex[c];
      }
      while(--count);
    }
    else
      str += '0';

    str += '.';

    // FRACTIONAL PART

    double fp = f - floor(f);
    do
    {
      int c = (int)(fp * base);
      fp = fp * base - c;

      if (c<0 || c>15)
      {
        return "#ERR";
      }

      str += hex[c];
    }
    while(fp>0);

    // COMMON PIPELINE

    // (1) EXPONENTIAL SHIFT
    // (2) CLIP & FILL DECIMALS
    // (3) EXPONENTIAL DECORATIONS
    // (4) EURO NOTATION
    // (5) FIELD, ALIGN AND SIGN
    // (6) CASE TRANSFORM

    return pipeline(str, base, field, decimals, finalizer, align, eur, fill, negative, plus);
  }
}

String Say::pipeline(const String& in_str, int base, int field, int decimals, int finalizer, int align, int eur, int fill, int negative, int plus) const
{
  String str = in_str;
  // EXPONENTIAL SHIFT

  int shift = 0;
  // exponential notation
  if (finalizer == 'e' || finalizer == 'E')
  {
    int ptpos = (int)str.length(); // point position
    int nzpos = -1; // non zero position
    for(int i=0; i<(int)str.length(); ++i)
    {
      if(str[i] != '0' && nzpos == -1 && str[i] != '.')
        nzpos = i;
      else
      if (str[i] == '.')
        ptpos = i;
    }

    if (nzpos == -1)
      shift = 0;
    else
      shift = ptpos - nzpos - ( (ptpos > nzpos) ? 1 : 0 );

    // remove the point
    str.remove( ptpos, 1 );

    // remove all the zeros on the left
    while( str.length() && str[0] == '0' )
      str.remove(0);

    // reinsert the point at the 2-nd position
    // with up to 2 zero if needed.
    if (str.length() == 1)
      str += '0';
    if (str.length() == 0)
      str = "00";

    str.insert(1, '.');
  }

  // CLIP AND FILL DECIMALS

  // position of the dot
  if ( !str.contains('.') )
    str += ".0";
  int pos = str.find('.');
  // number of decimals
  int decs = (int)str.length() - pos -1;
  // trim decimals
  if (decs > decimals)
  {
    // remove also the dot
    int dot = decimals == 0 ? 1 : 0;
    str.resize(str.length() - (decs - decimals + dot));
  }
  else
  {
    // add missing decimals
    int i = decimals - decs;
    while(i--)
      str += '0';
  }

  // EXPONENTIAL DECORATION

  if (finalizer == 'e' || finalizer == 'E')
  {
    str += 'e';
    str += format((signed long long)shift, base, 0, 0, 2, 0, 1, 0,0);
  }
  else
  // EURO NOTATION

  if (eur)
    str = euronotation(str, base);

  // FIELD, SIGN, ALIGN

  int right = (field - (int)str.length()) / 2;
  right = right < 0 ? 0 : right;

  int left =  (field - (int)str.length()) - right;
  left = left < 0 ? 0 : left;

  if (align == 1) // left
  {
    right += left;
    left = 0;
  }
  else
  if (align == 2) // right
  {
    left += right;
    right = 0;
  }

  // fill left
  str.insert(0, (wchar_t)fill, left);

  // fill right
  str.append(fill, right);

  if (negative)
  {
    if (left)
      str.remove(0);
    else
    if (right)
      str.resize(str.length()-1);

    str.insert(0, '-');
  }
  else
  if(plus)
  {
    if (left)
      str.remove(0);
    else
    if (right)
      str.resize(str.length()-1);

    str.insert(0, '+');
  }

  // CASE TRANSFORM

  if (finalizer == 'N' || finalizer == 'E')
  {
    for(int i=0; i<(int)str.length(); ++i)
      if (str[i] >= 'a' && str[i] <= 'z')
        str[i] = str[i] - 'a' + 'A';
  }

  return str;
}

