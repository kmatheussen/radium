varying vec4 vertex_position;

void main(){
  gl_Position = ftransform();
  //vertex_position = gl_ModelViewMatrix*gl_Vertex;
  //vertex_position = vec3(gl_ModelViewMatrix * gl_Vertex); //gl_Vertex * gl_ModelViewProjectionMatrix;
  //vertex_position = gl_ModelViewProjectionMatrix;
  vertex_position = gl_ModelViewMatrix * gl_Position; //Vertex;
}
