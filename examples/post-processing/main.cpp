////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Demo application for testing post processors. Loads image from
/// file, performs post processing and then saves file to disk
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#include <cstdio>

#include <xen/core/memory/Allocator.hpp>
#include <xen/sren/PostProcessor.hpp>
#include <xen/sren/Framebuffer.hpp>

#include <xen/graphics/Image.hpp>

int main(int argc, const char** argv){

	if(argc < 3){
		printf("ERROR - BAD USAGE\n");
		printf("Usage: %s InputFileName OutputFileName\n", argv[0]);
		return 1;
	}

	const char* filename_in  = argv[1];
	const char* filename_out = argv[2];

	xen::Allocator* alloc = new xen::AllocatorCounter<xen::AllocatorMalloc>();

	//////////////////////////////////////////////////////////////////////////////
	// Load input image
	xen::RawImage image_in  = xen::loadImage(*alloc, filename_in);
	if(image_in.size.x == 0 || image_in.size.y == 0 || image_in.pixels == nullptr){
		printf("Failed to open input image: %s\n", filename_in);
		return 1;
	}
	printf("Loaded input image\n");

	//////////////////////////////////////////////////////////////////////////////
	// Create output image
	xen::RawImage image_out = xen::createImage(*alloc, image_in.size);
  if(image_out.size.x == 0 || image_out.size.y == 0 || image_out.pixels == nullptr){
	  printf("Failed to allocate output image\n");
	  xen::destroyImage(*alloc, image_in);
	  return 1;
  }
  printf("Created output image\n");

  //////////////////////////////////////////////////////////////////////////////
	// Create framebuffer
  xsr::Framebuffer* fb = xsr::createFramebuffer(*alloc, image_in.size);
	if(fb == nullptr){
		xen::destroyImage(*alloc, image_in );
		xen::destroyImage(*alloc, image_out);
		printf("Failed to create framebuffer\n");
		return 1;
	}
	printf("Created framebuffer\n");

	//////////////////////////////////////////////////////////////////////////////
	// Do post processing
	printf("Performing processing... ");
	xsr::putImageOnFramebuffer(fb, image_in);
	xsr::PostProcessorInvertColors pp;
	pp.process(*fb);
	xsr::getImageFromFramebuffer(fb, image_out);
	printf("Complete\n");

	xen::saveImage(image_out, filename_out);
	printf("Wrote output image\n");

	//////////////////////////////////////////////////////////////////////////////
	// Clean up
	xen::destroyImage(*alloc, image_in );
	xen::destroyImage(*alloc, image_out);
	xsr::destroyFramebuffer(*alloc, fb);
	delete alloc;

	return 0;
}
