#pragma once

extern void RT_memory_freer_init(void);
extern bool RT_free(void *mem);

#if __cplusplus
namespace radium {
  struct Deletable{
    virtual ~Deletable()
    {
    }
  };
}

bool RT_free(radium::Deletable *mem);

#endif
