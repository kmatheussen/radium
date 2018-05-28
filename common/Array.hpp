

namespace radium{

template <typename T>
struct Array{

private:
  
  T *_elements = NULL;  
  int _num_elements = 0;

public:
  
  Array(){
  }

  Array(int num_elements){
    init(num_elements);
  }
  
  ~Array(){
    V_free(_elements);
  }

  void init(int num_elements){
    //R_ASSERT(_elements==NULL);
    R_ASSERT_RETURN_IF_FALSE(num_elements < 9999999);
    
    V_free(_elements);
    _num_elements = num_elements;
    _elements = (T*)V_calloc(num_elements, sizeof(T));
  }

  T operator[](int i) const {
    return _elements[i];
  }

  void set(int i, T value){
    _elements[i] = value;
  }

  int size(void){
    return _num_elements;
  }
  
  T *get_array(void){
    return _elements;
  }
  
  const T* begin() const {
    return &_elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    return &_elements[_num_elements];
  }

};

  #if 0
template <typename T>
struct Array2D{

private:
  
  Array<T> *_elements = NULL;
  int _num_elements1 = 0;
  int _num_elements2 = 0;

public:
  
  Array(){
  }

  Array(int num_elements1, int num_elements2){
    init(num_elements1, num_elements2);
  }
  
  ~Array(){
    for(auto *element : _elements)
      delete element;
    
    V_free(_elements);
  }

  void init(int num_elements){
    R_ASSERT(_elements==NULL);
    R_ASSERT_RETURN_IF_FALSE(num_elements < 9999999);
    _num_elements = num_elements;
    _elements = (T*)V_calloc(num_elements, sizeof(T));
  }

  T operator[](int i) const {
    return _elements[i];
  }

  void set(int i, T value){
    _elements[i] = value;
  }

  int size(void){
    return _num_elements;
  }
  
  T *get_array(void){
    return _elements;
  }
  
  const T* begin() const {
    return &_elements[0];
  }

  // This function can be called in parallell with the other const functions (i.e. the non-mutating ones).
  const T* end() const {
    return &_elements[_num_elements];
  }

};
#endif
  
}

