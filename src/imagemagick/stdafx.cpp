// stdafx.cpp : source file that includes just the standard includes
// imagemagick.pch will be the pre-compiled header
// stdafx.obj will contain the pre-compiled type information

#include "stdafx.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h>
#include <sys\types.h>
#include <time.h>

extern "C"
{

#include "..\ieplugin.h"

#include <wand\magick-wand.h>

// global data
int plugincapability[28] = {
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // PCD (Kodak PCD)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // DCM (DICOM)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // CUT (DR HALO CUT)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // AVS (AVS)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // CIN (Kodak Cineon)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // DOT (Graphviz DOT)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // DPX (SMTPE DPX)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // FITS (FITS)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // FPX (FlashPIX)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // HDF (HDF)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // MAT (Matlab)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // MIFF (MIFF)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // MTV (MTV Raytracer)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // PALM (Palm Pixmap)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // PCL (HP PCL Printer)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // PICT (Apple PICT)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // PIX (PIX)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // PWP (PWP)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // RLA (RLA)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // SGI (Irix RGB)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // SUN (SUN Rasterfile)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // SVG (Scalable Vector Graphic)
                          IEX_FILEREADER | IEX_MULTITHREAD,                   // TTF (TrueType/Postscript font)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // VICAR (VICAR)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // VIFF (Khoros VIFF)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // XBM (XBM)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // XCF (GUMP XCF)
                          IEX_FILEREADER | IEX_FILEWRITER | IEX_MULTITHREAD,  // XPM (XPM)
                          };
char* plugindescriptor[28] = {
                            "Kodak PCD",  
                            "DICOM",      
                            "DR HALO CUT",
                            "AVS",
                            "Kodak Cineon",
                            "Graphviz DOT",
                            "SMTPE DPX",
                            "FITS",
                            "FlashPIX",
                            "HDF",
                            "Matlab",
                            "MIFF",
                            "MTV Raytracer",
                            "Palm Pixmap",
                            "HP PCL Printer",
                            "Apple PICT",
                            "PIX",
                            "PWP",
                            "RLA",
                            "Irix RGB",
                            "SUN Rasterfile",
                            "Scalable Vector Graphic",
                            "TrueType/Postscript font",
                            "VICAR",
                            "Khoros VIFF",
                            "XBM",
                            "GUMP XCF",
                            "XPM"
                            };
char* pluginextensions[28] = {
                            "PCD",       
                            "DCM;DICOM",
                            "CUT",
                            "AVS",
                            "CIN",
                            "DOT",
                            "DPX",
                            "FITS",
                            "FPX",
                            "HDF",
                            "MAT",
                            "MIFF",
                            "MTV",
                            "PALM",
                            "PCL",
                            "PICT",
                            "PIX",
                            "PWP",
                            "RLA",
                            "SGI",
                            "SUN",
                            "SVG",
                            "TTF",
                            "VICAR",
                            "VIFF",
                            "XBM",
                            "XCF",
                            "XPM"
                            };
char* extmagick[28] = {
                            "PCD",       
                            "DCM",
                            "CUT",
                            "AVS",
                            "CIN",
                            "DOT",
                            "DPX",
                            "FITS",
                            "FPX",
                            "HDF",
                            "MAT",
                            "MIFF",
                            "MTV",
                            "PALM",
                            "PCL",
                            "PICT",
                            "PIX",
                            "PWP",
                            "RLA",
                            "SGI",
                            "SUN",
                            "SVG",
                            "TTF",
                            "VICAR",
                            "VIFF",
                            "XBM",
                            "XCF",
                            "XPM"
                            };
bool autosearchexif[28] =    {
                            false,        
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false,
                            false
                            };
int formatsCount=28;

//---------------------------------------------------------------------------

__declspec(dllexport) void* WINAPI IEX_GetInfo(void* handle, int info)
{
  IEX_Info* ihandle = (IEX_Info*)handle;
  switch( info )
  {
    case IEX_IMAGEWIDTH:
      return &ihandle->imWidth;
    case IEX_IMAGEHEIGHT:
      return &ihandle->imHeight;
    case IEX_PIXELFORMAT:
      return &ihandle->imFormat;
    case IEX_FORMATDESCRIPTOR:
      return ihandle->imFormatDescriptor;
    case IEX_IMAGEDATA:
      return ihandle->imData;
    case IEX_IMAGEPALETTE:
      return ihandle->imPalette;

    case IEX_PLUGINCAPABILITY:
      return &plugincapability[ihandle->selectedFormat];
    case IEX_PLUGINDESCRIPTOR:
      return plugindescriptor[ihandle->selectedFormat];
    case IEX_FILEEXTENSIONS:
      return pluginextensions[ihandle->selectedFormat];
    case IEX_AUTOSEARCHEXIF:
      return &autosearchexif[ihandle->selectedFormat];
    case IEX_FORMATSCOUNT:
      return &formatsCount;
    
    default:
      return 0;
		
  }
}

__declspec(dllexport) void WINAPI IEX_SetInfo(void* handle, int info, void* value)
{
  IEX_Info* ihandle = (IEX_Info*)handle;
  switch( info )
  {
    case IEX_IMAGEWIDTH:
      ihandle->imWidth = *(int*)value;
      break;
    case IEX_IMAGEHEIGHT:
      ihandle->imHeight = *(int*)value;
      break;
    case IEX_PIXELFORMAT:
      ihandle->imFormat = *(int*)value;
      break;
    case IEX_IMAGEDATA:
      ihandle->imData = value;
      break;
    case IEX_IMAGEPALETTE:
      ihandle->imPalette = value;
      break;
  }
}

__declspec(dllexport) void WINAPI IEX_SetCallBacks(void* handle, TIEX_Progress progressfun, TIEX_Read readfun, TIEX_Write writefun, TIEX_PositionSet posset, TIEX_PositionGet posget, TIEX_Length lenfun, TIEX_GetParameter getparam, TIEX_SetParameter setparam, void* userData )
{
  IEX_Info* ihandle = (IEX_Info*)handle;
  ihandle->userData=userData;
  ihandle->IEX_Progress=(TIEX_Progress)progressfun;
  ihandle->IEX_Read=(TIEX_Read)readfun;
  ihandle->IEX_Write=(TIEX_Write)writefun;
  ihandle->IEX_PositionSet=(TIEX_PositionSet)posset;
  ihandle->IEX_PositionGet=(TIEX_PositionGet)posget;
  ihandle->IEX_Length=(TIEX_Length)lenfun;
  ihandle->IEX_GetParameter=(TIEX_GetParameter)getparam;
  ihandle->IEX_SetParameter=(TIEX_SetParameter)setparam;
}

__declspec(dllexport) void* WINAPI IEX_Initialize(int format)
{
  IEX_Info* ihandle = (IEX_Info*)malloc(sizeof *ihandle);
  ihandle->IEX_Progress=0;
  ihandle->IEX_Read=0;
  ihandle->IEX_Write=0;
  ihandle->IEX_PositionSet=0;
  ihandle->IEX_PositionGet=0;
  ihandle->IEX_Length=0;
  ihandle->IEX_GetParameter=0;
  ihandle->IEX_SetParameter=0;
  ihandle->imWidth=0;
  ihandle->imHeight=0;
  ihandle->imFormat=IEX_INVALID;
  ihandle->imFormatDescriptor[0]=0;
  ihandle->imData=0;
  ihandle->imPalette=0;
  ihandle->userData=0;
  ihandle->plugInData=0;
  ihandle->plugInData1=0;
  ihandle->selectedFormat=format;
  return ihandle;
}

__declspec(dllexport) void WINAPI IEX_AddParameter(void* handle, char* param)
{
}

__declspec(dllexport) void WINAPI IEX_Finalize(void* handle)
{
  IEX_Info* ihandle = (IEX_Info*)handle;
  switch( ihandle->plugInData1 )
  {
    case 1:
      // free decoder
       free( ihandle->imData );
      break;
    case 2:
      // free encoder
      break;
  }
  free(handle);
}

__declspec(dllexport) void WINAPI IEX_ExecuteRead(void* handle, bool parametersOnly)
{
  IEX_Info* ihandle = (IEX_Info*)handle;

  MagickBooleanType status;
  MagickWand *wand = NewMagickWand();  

  if(parametersOnly)
  {
    ihandle->plugInData1 = 0;

    int len=(*ihandle->IEX_Length)(ihandle->userData);
    unsigned char* ptr=(unsigned char*)malloc(len);
    (*ihandle->IEX_Read)(ptr, len, ihandle->userData);
    status = MagickReadImageBlob( wand, ptr, len);  
    free(ptr);

    ihandle->imWidth = MagickGetImageWidth(wand);
    ihandle->imHeight = MagickGetImageHeight(wand);

    int imagedepth = MagickGetImageDepth(wand);
    
    char ss[128];
	  (*ihandle->IEX_GetParameter)("ImageIndex", ss, ihandle->userData);
	  int imageindex = atol(ss);
	  for(int i=0; i<imageindex; i++)
		  MagickNextImage(wand);

    ihandle->imFormat=IEX_24RGB;
    ihandle->imData=0;

    char s[64];
    double dpix,dpiy;
    MagickGetImageResolution(wand,&dpix,&dpiy);
    (*ihandle->IEX_SetParameter)("DpiX", _ltoa(dpix,s,10), ihandle->userData);
    (*ihandle->IEX_SetParameter)("DpiY", _ltoa(dpix,s,10), ihandle->userData);

    int imcount = MagickGetNumberImages(wand);
    (*ihandle->IEX_SetParameter)("ImageCount", _ltoa(imcount,s,10), ihandle->userData);

  }
  else
  {
    ihandle->plugInData1 = 1;

    int len=(*ihandle->IEX_Length)(ihandle->userData);
    unsigned char* ptr=(unsigned char*)malloc(len);
    (*ihandle->IEX_Read)(ptr, len, ihandle->userData);
    status = MagickReadImageBlob( wand, ptr, len);  
    free(ptr);

    ihandle->imWidth = MagickGetImageWidth(wand);
    ihandle->imHeight = MagickGetImageHeight(wand);

    int imagedepth = MagickGetImageDepth(wand);
    
    char ss[128];
	  (*ihandle->IEX_GetParameter)("ImageIndex", ss, ihandle->userData);
	  int imageindex = atol(ss);
	  for(int i=0; i<imageindex; i++)
		  MagickNextImage(wand);

    /*
    if(imagedepth==8)
    {
       ihandle->imFormat=IEX_8G;
       ihandle->imData=(unsigned char*)malloc((ihandle->imWidth)* ihandle->imHeight );
       MagickGetImagePixels(wand,0,0,ihandle->imWidth,ihandle->imHeight,"I",CharPixel,ihandle->imData);
    }
    else
    */
    {
      ihandle->imFormat=IEX_24RGB;
      ihandle->imData=(unsigned char*)malloc(ihandle->imWidth * ihandle->imHeight * 3);
      MagickSetImageColorspace(wand,RGBColorspace);
      MagickGetImagePixels(wand,0,0,ihandle->imWidth,ihandle->imHeight,"RGB",CharPixel,ihandle->imData);
    }

    char s[64];
    double dpix,dpiy;
    MagickGetImageResolution(wand,&dpix,&dpiy);
    (*ihandle->IEX_SetParameter)("DpiX", _ltoa(dpix,s,10), ihandle->userData);
    (*ihandle->IEX_SetParameter)("DpiY", _ltoa(dpix,s,10), ihandle->userData);

    int imcount = MagickGetNumberImages(wand);
    (*ihandle->IEX_SetParameter)("ImageCount", _ltoa(imcount,s,10), ihandle->userData);

  }

  strcpy(ihandle->imFormatDescriptor,plugindescriptor[ihandle->selectedFormat]);

  ihandle->imPalette=0;

  DestroyMagickWand(wand);

}

__declspec(dllexport) bool WINAPI IEX_ExecuteTry(void* handle)
{
  IEX_Info* ihandle = (IEX_Info*)handle;

  ihandle->plugInData1 = 0;

  int len=(*ihandle->IEX_Length)(ihandle->userData);
  unsigned char* ptr=(unsigned char*)malloc(len);
  (*ihandle->IEX_Read)(ptr, len, ihandle->userData);

  const MagickInfo* mi;
  int r = false;

  mi=GetMagickInfo(extmagick[ihandle->selectedFormat],0);
  if(mi)
  {
    IsImageFormatHandler * trythis = mi->magick;
    if(trythis) 
      r= trythis(ptr,len) == MagickTrue;
  }

  free(ptr);

  return r;
}

__declspec(dllexport) void WINAPI IEX_ExecuteWrite(void* handle)
{
  IEX_Info* ihandle = (IEX_Info*)handle;

  ihandle->plugInData1 = 2;
  
  if( ihandle->imFormat == IEX_1G )
  {
  }
  else if ( ihandle->imFormat == IEX_24RGB )
  {
    MagickWand *wand = NewMagickWand();

    MagickConstituteImage(wand,ihandle->imWidth,ihandle->imHeight,"RGB",CharPixel,ihandle->imData);

    MagickSetImageFormat(wand, extmagick[ihandle->selectedFormat]);

    size_t len;
    unsigned char* blob = MagickGetImageBlob(wand, &len);
    (*ihandle->IEX_Write)(blob, (int)len, ihandle->userData);
    
    MagickRelinquishMemory(blob);

    DestroyMagickWand(wand);
  }
}



}
