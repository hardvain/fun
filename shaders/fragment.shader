#version 410 core

// Interpolated values from the vertex shaders
in vec4 fragmentColor;

// Ouput data
out vec4 fColor;

void
main()
{
   fColor =  fragmentColor;
}