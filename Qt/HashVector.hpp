#ifndef _RADIUM_QT_HASH_VECTOR_HPP
#define _RADIUM_QT_HASH_VECTOR_HPP 1


#include <QHash>
#include <QVector>

#if defined(RELEASE)
#  define DO_VALIDATE 0
#else
#  define DO_VALIDATE 1
#endif

namespace radium{

// Fast lookup and fast iteration.
// insert/delete is quite fast, and O(1).
template <typename K, typename T> class HashVector {
  
  QVector<T> _vector;
  QHash<K,int> _hash;
  QVector<K> _inv_hash;

  void validate(void) const {
#if DO_VALIDATE
    
    if (_vector.size() != _hash.size())
      abort();

    if (_hash.size() != _inv_hash.size())
      abort();

    QSet<K> keys1 = _hash.keys().toSet();
    QSet<K> keys2 = _inv_hash.toList().toSet();
    if (keys1 != keys2)
      abort();

    for(int i=0;i<_vector.size();i++){
      K key = _inv_hash[i];

      if (!_hash.contains(key))
        abort();
      
      int pos = _hash[key];

      if (pos != i)
        abort();
    }    
#endif
  }
  
public:

  int size() const {
    return _vector.size();
  }
  
  T const *begin() const {
    return _vector.begin(); //&_vector.constFirst();
  }
  
  T const *end() const {
    return _vector.end(); //&_vector.constLast();
  }

  /*
  const T* const begin() const {
    return _vector.begin();
  }
  
  const T* const end() const {
    return _vector.end();
  }
  */
  void put(const K &key, const T &t){
    int size = _vector.size();
    _hash[key] = size;
    _inv_hash.push_back(key);
    _vector.push_back(t);
    validate();
  }

  bool remove(const K &key){
    int pos = _hash.value(key, -1);
      
    if(pos==-1)
      return false;
    
    int last_pos = _vector.size()-1;
    K last_key = _inv_hash[last_pos];

    R_ASSERT_RETURN_IF_FALSE2(pos>=0,false);
    R_ASSERT_RETURN_IF_FALSE2(pos<=last_pos,false);
    
    if (pos != last_pos){
      _hash[last_key] = pos;
      _inv_hash[pos] = _inv_hash[last_pos];
      _vector[pos] = _vector[last_pos];
    }

    int num_removed = _hash.remove(key);
    R_ASSERT(num_removed==1);
    
    _vector.removeLast();
    _inv_hash.removeLast();

    validate();
    return true;
  }

  /*
  T &get(const K &key) const {
    int pos = _hash.value(key, -1);
    if (pos==-1){
      R_ASSERT(false);
      return _vector.at(0);
    }
    
    return _vector.at(_hash[key]);
  }
  */
  
  const T &get(const K &key) const {
    int pos = _hash.value(key, -1);
    if (pos==-1){
      R_ASSERT(false);
      return _vector.at(0);
    }
    
    return _vector.at(_hash[key]);
  }

  T &get(const K &key, T &default_value) const {
    int pos = _hash.value(key, -1);
    if (pos==-1)
      return NULL;//default_value;
    
    //fprintf(stderr, "1pos: %d. size: %d\n", pos, _vector.size());
    return _vector.at(pos);
  }

  const T &get(const K &key, const T &default_value) const {
    int pos = _hash.value(key, -1);
    if (pos==-1)
      return default_value;

    //fprintf(stderr, "2pos: %d. size: %d\n", pos, _vector.size());
    return _vector.at(pos);
  }

  bool contains(const K &key) const {
    return _hash.contains(key);
  }
};

}


#undef DO_VALIDATE

#endif
