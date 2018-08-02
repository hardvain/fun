#version 410 core

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;
uniform mat4 transform;

out vec4 fragmentColor;

void main() {
  gl_Position = transform * position;
  fragmentColor = color;
}
