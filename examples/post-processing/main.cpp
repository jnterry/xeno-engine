////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Demo application for testing post processors. Loads image from
/// file, performs post processing and then saves file to disk
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#include <cstdio>
#include "../common.hpp"

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

	xen::RawImage image_in  = xen::loadImage(*alloc, filename_in);
	xen::sren::Framebuffer* fb = xen::sren::createFramebuffer(*alloc, image_in.size);
	xen::sren::putImageOnFramebuffer(fb, image_in);

	xen::sren::PostProcessorInvertColors pp;
	//pp.process(*fb);

	xen::RawImage image_out = xen::createImage(*alloc, image_in.size);
	xen::sren::getImageFromFramebuffer(fb, image_out);

	xen::saveImage(image_out, filename_out);

	xen::destroyImage(*alloc, image_in );
	xen::destroyImage(*alloc, image_out);
	xen::sren::destroyFramebuffer(*alloc, fb);

	delete alloc;

	return 0;
}
