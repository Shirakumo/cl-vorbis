#include "stb_vorbis.c"

extern void stb_vorbis_get_info_(stb_vorbis *f, stb_vorbis_info *i);
extern void stb_vorbis_get_comment_(stb_vorbis *f, stb_vorbis_comment *i);

void stb_vorbis_get_info_(stb_vorbis *f, stb_vorbis_info *i){
  stb_vorbis_info info = stb_vorbis_get_info(f);
  memcpy(i, &info, sizeof(info));
}

void stb_vorbis_get_comment_(stb_vorbis *f, stb_vorbis_comment *i){
  stb_vorbis_comment info = stb_vorbis_get_comment(f);
  memcpy(i, &info, sizeof(info));
}
