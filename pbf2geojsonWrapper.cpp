#include <Rcpp.h>
#undef Realloc
#undef Free
#include <Windows.h>

using namespace Rcpp;
typedef char* WINAPI (*TdecodeMvtProc)(const int zoom, const int col, const int row, const double tileX, const double tileY, const double tileSpanX, const double tileSpanY, const char* data);

// [[Rcpp::export]]
std::string decodePBF(const int zoom, const int col, const int row, const double tileX, const double tileY, const double tileSpanX, const double tileSpanY, const char* data) {
  HMODULE dll = LoadLibrary("pbf2geojson/pbf2geojson_windows_x86_64.dll");
  if (dll == NULL)
  {
    std::puts("fail");
    return 0;
  }
  FARPROC proc = GetProcAddress(dll, "decodeMvtToJson");
  if (proc == NULL)
  {
    std::puts("fail");
    return 0;
  }
  TdecodeMvtProc decodeMvt = reinterpret_cast<TdecodeMvtProc>(proc);
  char* ret= decodeMvt(zoom,col,row,tileX,tileY,tileSpanX,tileSpanY,data);
  FreeLibrary(dll);
  return ret;
}


