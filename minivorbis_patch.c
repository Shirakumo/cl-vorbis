#define OGG_IMPL
#define VORBIS_IMPL
#include "minivorbis.c"

extern int ov_open_callbacks_(void *datasource, OggVorbis_File *vf,
                              const char *initial, long ibytes, ov_callbacks *callbacks);
extern int ov_test_callbacks_(void *datasource, OggVorbis_File *vf,
                              const char *initial, long ibytes, ov_callbacks *callbacks);

int ov_open_callbacks_(void *datasource, OggVorbis_File *vf,
                       const char *initial, long ibytes, ov_callbacks *callbacks){
  ov_callbacks _callbacks = {callbacks->read_func, callbacks->seek_func, callbacks->close_func, callbacks->tell_func};
  return ov_open_callbacks(datasource, vf, initial, ibytes, _callbacks);
}

int ov_test_callbacks_(void *datasource, OggVorbis_File *vf,
                       const char *initial, long ibytes, ov_callbacks *callbacks){
  ov_callbacks _callbacks = {callbacks->read_func, callbacks->seek_func, callbacks->close_func, callbacks->tell_func};
  return ov_test_callbacks(datasource, vf, initial, ibytes, _callbacks);
}
