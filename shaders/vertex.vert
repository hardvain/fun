#version 410 core

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

uniform vec3 translate;
uniform vec3 rotate;

out vec4 fragmentColor;


mat4 build_transform(vec3 pos, vec3 ang) 
{
  float cosX = cos(ang.x);
  float sinX = sin(ang.x);
  float cosY = cos(ang.y);
  float sinY = sin(ang.y);
  float cosZ = cos(ang.z);
  float sinZ = sin(ang.z);

  mat4 m;

  float m00 = cosY * cosZ + sinX * sinY * sinZ; 
  float m01 = cosY * sinZ - sinX * sinY * cosZ; 
  float m02 = cosX * sinY;
  float m03 = 0.0;
  
  float m04 = -cosX * sinZ; 
  float m05 = cosX * cosZ; 
  float m06 = sinX;
  float m07 = 0.0;
  
  float m08 = sinX * cosY * sinZ - sinY * cosZ;
  float m09 = -sinY * sinZ - sinX * cosY * cosZ;
  float m10 = cosX * cosY;
  float m11 = 0.0;
  
  float m12 = pos.x;
  float m13 = pos.y;
  float m14 = pos.z;
  float m15 = 1.0;

  /*
  //------ Orientation ---------------------------------
  m[0] = vec4(m00, m01, m02, m03); // first column.
  m[1] = vec4(m04, m05, m06, m07); // second column.
  m[2] = vec4(m08, m09, m10, m11); // third column.

  //------ Position ------------------------------------
  m[3] = vec4(m12, m13, m14, m15); // fourth column.
  */

  //------ Orientation ---------------------------------
  m[0][0] = m00; // first entry of the first column.
  m[0][1] = m01; // second entry of the first column.
  m[0][2] = m02;
  m[0][3] = m03;
  
  m[1][0] = m04; // first entry of the second column.
  m[1][1] = m05; // second entry of the second column.
  m[1][2] = m06;
  m[1][3] = m07;

  m[2][0] = m08; // first entry of the third column.
  m[2][1] = m09; // second entry of the third column.
  m[2][2] = m10;
  m[2][3] = m11;
  
  //------ Position ------------------------------------
  m[3][0] = m12; // first entry of the fourth column.
  m[3][1] = m13; // second entry of the fourth column.
  m[3][2] = m14;
  m[3][3] = m15;

  return m;
}

void main() {
  gl_Position = build_transform(translate,rotate) * position;
  fragmentColor = color;
}
