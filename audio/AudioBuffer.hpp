
#ifndef _RADIUM_AUDIO_AUDIOBUFFER_HPP
#define _RADIUM_AUDIO_AUDIOBUFFER_HPP

namespace radium{

  /*
class AudioBuffer{
  float **_samples;
  int _num_ch;
  int _num_frames;
  AudioBuffer(float **samples, int num_ch, int num_frames)
    : _samples(samples)
    , _num_ch(num_ch)
    , _num_frames(num_frames)
  {
  }
  void make_clean(void) const {
    for(int ch=0;ch<_num_ch;ch++)
      memset(_samples[ch], 0, sizeof(float)*_num_frames);
  }
  void skew(int dx){
    if (dx > _num_frames){
      R_ASSERT(false);
      dx = _num_frames;
    } else {
      R_ASSERT_NON_RELEASE(dx < _num_frames);
    }

    for(int ch=0;ch<_num_ch;ch++){
      _samples[ch] += dx;
    }
    _num_frames -= dx;
  }
};
  */


  enum class NeedsLock{
    YES,NO
      };

  extern "C"{
  struct AudioBufferChannel{
    union{
      AudioBufferChannel *next;
      float buffer[64]; // Note: The allocated size is actually RADIUM_BLOCKSIZE. We just set 64 to avoid compilation errors.
    };
  };
  }
}

extern void AUDIOBUFFERS_init(void);

extern void RT_AUDIOBUFFERS_optimize(void);

extern void RT_AUDIOBUFFER_release_channel(radium::AudioBufferChannel *buffer, radium::NeedsLock needs_lock); // Use release_channels if releasing more than one, to avoid fragmentation.
extern void RT_AUDIOBUFFER_release_channels(radium::AudioBufferChannel **channels, int num_ch, radium::NeedsLock needs_lock);

extern radium::AudioBufferChannel *RT_AUDIOBUFFER_get_channel(radium::NeedsLock needs_lock)  __attribute__((returns_nonnull)); // User get_channels if allocating more than one, to avoid fragmentation.
extern void RT_AUDIOBUFFER_get_channels(radium::AudioBufferChannel **channels, int num_channels, radium::NeedsLock needs_lock);

namespace radium{

  class AudioBufferChannelStorage{
    AudioBufferChannel *_channels = NULL;
    int _num_elements = 0;
    
  public:
    
    int size(void) const{
      return _num_elements;
    }
    
    AudioBufferChannel *get(radium::NeedsLock needs_lock){
      if (_channels==NULL)
        return RT_AUDIOBUFFER_get_channel(needs_lock);

      _num_elements--;
      
      auto *ret = _channels;
      _channels = _channels->next;      
      
      return ret;
    }

    void put(AudioBufferChannel *channel){
      _num_elements++;
      
      channel->next = _channels;
      _channels = channel;
    }

    void release_all(radium::NeedsLock needs_lock){
      while(_channels != NULL){
        auto *next = _channels->next;

        RT_AUDIOBUFFER_release_channel(_channels, needs_lock);
        
        _channels = next;
      }

      _num_elements = 0;
    }
    
    ~AudioBufferChannelStorage(){
      R_ASSERT(_channels==NULL);
    }
  };    
  
  struct AudioBuffer{
    const int _num_ch;
    
  private:
    bool _has_channels = false;
    AudioBufferChannel **_channels; // also const.
    
  public:
    
    AudioBuffer(int num_ch)
      : _num_ch(num_ch)
      , _channels(new AudioBufferChannel*[num_ch])
    {
      R_ASSERT(_num_ch >= 0);      
      
      for(int ch=0;ch<_num_ch;ch++)
        _channels[ch] = NULL;
    }

    ~AudioBuffer(){
      for(int ch=0;ch<_num_ch;ch++)
        R_ASSERT(_channels[ch] == NULL);
      
      delete[] _channels;
    }

    bool has_channels(void) const{
      return _has_channels;
    }
    
    float *get_channel(int ch) const{
      R_ASSERT_NON_RELEASE(_has_channels);
      return _channels[ch]->buffer;
    }

    // This one is probably 100% safe. (it better be, because it's used in the core of the audio engine)
    float **get_channels(void) const{
      R_ASSERT_NON_RELEASE(_has_channels);
      return (float**)_channels;
    }

    void clean(void){
      for(int ch=0;ch<_num_ch;ch++)
        memset(_channels[ch]->buffer, 0, sizeof(float)*RADIUM_BLOCKSIZE);
    }
    
    void RT_obtain_channels(radium::NeedsLock needs_lock){
      RT_AUDIOBUFFER_get_channels(_channels, _num_ch, needs_lock);
      _has_channels = true;
    }
    
    bool RT_obtain_channels_if_necessary(radium::NeedsLock needs_lock){
      if(_has_channels)
        return false;
      
      RT_obtain_channels(needs_lock);
      return true;
    }
    
    void RT_obtain_channels(AudioBufferChannelStorage &storage, radium::NeedsLock needs_lock){
      for(int ch=0;ch<_num_ch;ch++){
        R_ASSERT(_channels[ch] == NULL);
        _channels[ch] = storage.get(needs_lock);
      }
      _has_channels = true;
    }
    
    void RT_release_channels(radium::NeedsLock needs_lock){
      RT_AUDIOBUFFER_release_channels(_channels, _num_ch, needs_lock);
      _has_channels = false;
    }

    void RT_release_channels_if_necessary(radium::NeedsLock needs_lock){
      if(_has_channels)
        RT_release_channels(needs_lock);      
    }
      
    void RT_release_channels(AudioBufferChannelStorage &storage){
      for(int ch=_num_ch-1;ch>=0;ch--){ // Make sure the order of the individual channels is the same as when we started.
        
        if(_channels[ch] == NULL)
          R_ASSERT(false);
        else
          storage.put(_channels[ch]);
        
        _channels[ch] = NULL;
      }
      
      _has_channels = false;
    }

  };

}


#endif
