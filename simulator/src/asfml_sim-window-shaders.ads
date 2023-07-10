
private package ASFML_Sim.Window.Shaders is

   pragma Style_Checks (Off);

   Glow : constant String :=
     "#version 120" & ASCII.LF &
     "uniform sampler2D currentTexture;" & ASCII.LF &
     "uniform float sigma = 3.0;" & ASCII.LF &
     "uniform float glowMultiplier = 0.7;" & ASCII.LF &
     "uniform float width = 1.0015;" & ASCII.LF &
     "const int KERNEL_SIZE = 5;" & ASCII.LF &
     "float glow = glowMultiplier / (sigma * sqrt(2.0 * 3.14159));" & ASCII.LF &
     "float blurWeight(float x)" & ASCII.LF &
     "{" & ASCII.LF &
     "    return (glow * exp(-(x*x) / (2.0 * sigma * sigma)));" & ASCII.LF &
     "}" & ASCII.LF &
     "void main()" & ASCII.LF &
     "{" & ASCII.LF &
     "    vec4 color = vec4(0.0);" & ASCII.LF &
     "    vec2 texCoord = gl_TexCoord[0].xy;" & ASCII.LF &
     "    for (int i = -KERNEL_SIZE; i <= KERNEL_SIZE; i++)" & ASCII.LF &
     "    {" & ASCII.LF &
     "        for (int j = -KERNEL_SIZE; j <= KERNEL_SIZE; j++)" & ASCII.LF &
     "        {" & ASCII.LF &
     "            texCoord.x = gl_TexCoord[0].x + (i / width);" & ASCII.LF &
     "            texCoord.y = gl_TexCoord[0].y + (j / width);" & ASCII.LF &
     "            color += texture2D(currentTexture, texCoord) * blurWeight(sqrt (i*i + j*j));" & ASCII.LF &
     "        }" & ASCII.LF &
     "    }" & ASCII.LF &
     "    gl_FragColor = color + texture2D(currentTexture, gl_TexCoord[0].xy);" & ASCII.LF &
     "}" & ASCII.LF;

end ASFML_Sim.Window.Shaders;
