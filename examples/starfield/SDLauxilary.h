#ifndef SDL_AUXILIARY_H
#define SDL_AUXILIARY_H

#include <SDL.h>
#include <iostream>
#include <stdint.h>
#include <cstring>

#include <xen/graphics/Color.hpp>
#include <xen/graphics/Image.hpp>

typedef struct{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *texture;
  xen::RawImage buffer;

} screen;

screen* InitializeSDL( int width, int height, bool fullscreen = false );
bool NoQuitMessageSDL();
void PutPixelSDL( screen *s, u32 x, u32 y, xen::Color color );
void SDL_Renderframe(screen *s);
void KillSDL(screen* s);
void SDL_SaveImage(screen *s, const char* filename);

void SDL_SaveImage(screen *s, const char* filename)
{
  uint32_t rmask, gmask, bmask, amask;

  if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
      amask = 0xff << 0;
      rmask = 0xff << 8;
      gmask = 0xff << 16;
      bmask = 0xff << 24;
  } else {
	  amask = 0xff << 24;
	  rmask = 0xff << 16;
	  gmask = 0xff << 8;
	  bmask = 0xff << 0;
  }

  SDL_Surface* surf = SDL_CreateRGBSurfaceFrom((void*)s->buffer.pixels,
                                               s->buffer.width, s->buffer.height,
                                               32,
                                               s->buffer.width*sizeof(uint32_t),
                                               rmask,gmask,bmask,amask
                                              );
  if(SDL_SaveBMP(surf, filename) !=0)
    {
      std::cout << "Failed to save image: " << SDL_GetError() << std::endl;
      exit(1);
    }

}

void KillSDL(screen* s)
{
  delete[] s->buffer.pixels;
  SDL_DestroyTexture(s->texture);
  SDL_DestroyRenderer(s->renderer);
  SDL_DestroyWindow(s->window);
  SDL_Quit();
}

void SDL_Renderframe(screen* s)
{
	SDL_UpdateTexture(s->texture, NULL, s->buffer.pixels, s->buffer.width*sizeof(uint32_t));
  SDL_RenderClear(s->renderer);
  SDL_RenderCopy(s->renderer, s->texture, NULL, NULL);
  SDL_RenderPresent(s->renderer);
}

screen* InitializeSDL(int width, int height, bool fullscreen) {
  if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) !=0) {
	  std::cout << "Could not initialise SDL: " << SDL_GetError() << std::endl;
	  exit(1);
  }

  screen *s = new screen;
  s->buffer.width  = width;
  s->buffer.height = height;
  s->buffer.pixels = new xen::Color[width*height];

  memset(s->buffer.pixels, 0, width*height*sizeof(xen::Color));

  uint32_t flags = SDL_WINDOW_OPENGL;
  if(fullscreen) {
	  flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;
  }
  s->window = SDL_CreateWindow("COMS30115",
                               SDL_WINDOWPOS_UNDEFINED,
                               SDL_WINDOWPOS_UNDEFINED,
                               width, height,flags);
  if(s->window == 0) {
	  std::cout << "Could not set video mode: " << SDL_GetError() << std::endl;
	  exit(1);
  }

  flags = SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC;
  s->renderer = SDL_CreateRenderer(s->window, -1, flags);
  if(s->renderer == 0) {
	  std::cout << "Could not create renderer: " << SDL_GetError() << std::endl;
	  exit(1);
  }

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");
  SDL_RenderSetLogicalSize(s->renderer, width,height);

  s->texture = SDL_CreateTexture(s->renderer,
				 SDL_PIXELFORMAT_ARGB8888,
				 SDL_TEXTUREACCESS_STATIC,
				 s->buffer.width, s->buffer.height);
  if(s->texture==0) {
	  std::cout << "Could not allocate texture: " << SDL_GetError() << std::endl;
	  exit(1);
  }

  return s;
}

bool NoQuitMessageSDL()
{
  SDL_Event e;
  while( SDL_PollEvent(&e) ) {
	  if( e.type == SDL_QUIT ) {
		  return false;
	  }
	  if( e.type == SDL_KEYDOWN ) {
		  if( e.key.keysym.sym == SDLK_ESCAPE)
			  {
				  return false;
			  }
	  }
  }
  return true;
}

void PutPixelSDL(screen* s, u32 x, u32 y, xen::Color color){
	if(x >= 0 && y >= 0 && x < s->buffer.width && y < s->buffer.height){
		s->buffer[x][y] = color;
	}
}


#endif
