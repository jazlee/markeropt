/*
  Copyright 1999-2009 ImageMagick Studio LLC, a non-profit organization
  dedicated to making software imaging solutions freely available.
  
  You may not use this file except in compliance with the License.
  obtain a copy of the License at
  
    http://www.imagemagick.org/script/license.php
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  MagickCore API methods prefix.

  nm .libs/libMagickCore.a | grep ' T ' | \
    awk '{ printf("#define %s  PrependMagickMethod(%s)\n", $3, $3); }' | \
    sort
*/
#ifndef _MAGICKCORE_METHOD_H
#define _MAGICKCORE_METHOD_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#if defined(MAGICKCORE_NAMESPACE_PREFIX)

#if defined(__STDC__)
#define PrescanMagickPrefix(prefix,method)  prefix ## method
#else
#define PrescanMagickPrefix(prefix,method)  prefix(method)
#endif
#define EvaluateMagickPrefix(prefix,method)  PrescanMagickPrefix(prefix,method)
#define PrependMagickMethod(method) \
  EvaluateMagickPrefix(MAGICKCORE_NAMESPACE_PREFIX,method)

#define AcquireAlignedMemory  PrependMagickMethod(AcquireAlignedMemory)
#define AcquireCacheViewIndexes  PrependMagickMethod(AcquireCacheViewIndexes)
#define AcquireCacheViewPixels  PrependMagickMethod(AcquireCacheViewPixels)
#define AcquireCacheView  PrependMagickMethod(AcquireCacheView)
#define AcquireDrawInfo  PrependMagickMethod(AcquireDrawInfo)
#define AcquireExceptionInfo  PrependMagickMethod(AcquireExceptionInfo)
#define AcquireFxInfo  PrependMagickMethod(AcquireFxInfo)
#define AcquireImageColormap  PrependMagickMethod(AcquireImageColormap)
#define AcquireImageInfo  PrependMagickMethod(AcquireImageInfo)
#define AcquireImagePixels  PrependMagickMethod(AcquireImagePixels)
#define AcquireImage  PrependMagickMethod(AcquireImage)
#define AcquireIndexes  PrependMagickMethod(AcquireIndexes)
#define AcquireMagickMatrix  PrependMagickMethod(AcquireMagickMatrix)
#define AcquireMagickMemory  PrependMagickMethod(AcquireMagickMemory)
#define AcquireMagickResource  PrependMagickMethod(AcquireMagickResource)
#define AcquireMemory  PrependMagickMethod(AcquireMemory)
#define AcquireNextImage  PrependMagickMethod(AcquireNextImage)
#define AcquireOneCacheViewPixel  PrependMagickMethod(AcquireOneCacheViewPixel)
#define AcquireOneCacheViewVirtualPixel  PrependMagickMethod(AcquireOneCacheViewVirtualPixel)
#define AcquireOneMagickPixel  PrependMagickMethod(AcquireOneMagickPixel)
#define AcquireOnePixel  PrependMagickMethod(AcquireOnePixel)
#define AcquireOneVirtualPixel  PrependMagickMethod(AcquireOneVirtualPixel)
#define AcquirePixelCacheInfo  PrependMagickMethod(AcquirePixelCacheInfo)
#define AcquirePixelCacheNexus  PrependMagickMethod(AcquirePixelCacheNexus)
#define AcquirePixels  PrependMagickMethod(AcquirePixels)
#define AcquireQuantizeInfo  PrependMagickMethod(AcquireQuantizeInfo)
#define AcquireQuantumInfo  PrependMagickMethod(AcquireQuantumInfo)
#define AcquireQuantumMemory  PrependMagickMethod(AcquireQuantumMemory)
#define AcquireRandomInfo  PrependMagickMethod(AcquireRandomInfo)
#define AcquireResampleFilter  PrependMagickMethod(AcquireResampleFilter)
#define AcquireResizeFilter  PrependMagickMethod(AcquireResizeFilter)
#define AcquireSemaphoreInfo  PrependMagickMethod(AcquireSemaphoreInfo)
#define AcquireSignatureInfo  PrependMagickMethod(AcquireSignatureInfo)
#define AcquireStreamInfo  PrependMagickMethod(AcquireStreamInfo)
#define AcquireStringInfo  PrependMagickMethod(AcquireStringInfo)
#define AcquireString  PrependMagickMethod(AcquireString)
#define AcquireTimerInfo  PrependMagickMethod(AcquireTimerInfo)
#define AcquireTokenInfo  PrependMagickMethod(AcquireTokenInfo)
#define AcquireUniqueFilename  PrependMagickMethod(AcquireUniqueFilename)
#define AcquireUniqueFileResource  PrependMagickMethod(AcquireUniqueFileResource)
#define AcquireUniqueSymbolicLink  PrependMagickMethod(AcquireUniqueSymbolicLink)
#define AdaptiveBlurImageChannel  PrependMagickMethod(AdaptiveBlurImageChannel)
#define AdaptiveBlurImage  PrependMagickMethod(AdaptiveBlurImage)
#define AdaptiveResizeImage  PrependMagickMethod(AdaptiveResizeImage)
#define AdaptiveSharpenImageChannel  PrependMagickMethod(AdaptiveSharpenImageChannel)
#define AdaptiveSharpenImage  PrependMagickMethod(AdaptiveSharpenImage)
#define AdaptiveThresholdImage  PrependMagickMethod(AdaptiveThresholdImage)
#define AddChildToXMLTree  PrependMagickMethod(AddChildToXMLTree)
#define AddNoiseImageChannel  PrependMagickMethod(AddNoiseImageChannel)
#define AddNoiseImage  PrependMagickMethod(AddNoiseImage)
#define AddPathToXMLTree  PrependMagickMethod(AddPathToXMLTree)
#define AddValueToSplayTree  PrependMagickMethod(AddValueToSplayTree)
#define AffineTransformImage  PrependMagickMethod(AffineTransformImage)
#define AffinityImage  PrependMagickMethod(AffinityImage)
#define AffinityImages  PrependMagickMethod(AffinityImages)
#define AllocateImageColormap  PrependMagickMethod(AllocateImageColormap)
#define AllocateImage  PrependMagickMethod(AllocateImage)
#define AllocateNextImage  PrependMagickMethod(AllocateNextImage)
#define AllocateSemaphoreInfo  PrependMagickMethod(AllocateSemaphoreInfo)
#define AllocateString  PrependMagickMethod(AllocateString)
#define analyzeImage  PrependMagickMethod(analyzeImage)
#define AnimateImages  PrependMagickMethod(AnimateImages)
#define AnnotateImage  PrependMagickMethod(AnnotateImage)
#define AppendImageFormat  PrependMagickMethod(AppendImageFormat)
#define AppendImages  PrependMagickMethod(AppendImages)
#define AppendImageToList  PrependMagickMethod(AppendImageToList)
#define AppendValueToLinkedList  PrependMagickMethod(AppendValueToLinkedList)
#define Ascii85Encode  PrependMagickMethod(Ascii85Encode)
#define Ascii85Flush  PrependMagickMethod(Ascii85Flush)
#define Ascii85Initialize  PrependMagickMethod(Ascii85Initialize)
#define AsynchronousDestroyMagickResources  PrependMagickMethod(AsynchronousDestroyMagickResources)
#define AttachBlob  PrependMagickMethod(AttachBlob)
#define AverageImages  PrependMagickMethod(AverageImages)
#define Base64Decode  PrependMagickMethod(Base64Decode)
#define Base64Encode  PrependMagickMethod(Base64Encode)
#define BilevelImageChannel  PrependMagickMethod(BilevelImageChannel)
#define BilevelImage  PrependMagickMethod(BilevelImage)
#define BlackThresholdImageChannel  PrependMagickMethod(BlackThresholdImageChannel)
#define BlackThresholdImage  PrependMagickMethod(BlackThresholdImage)
#define BlobToFile  PrependMagickMethod(BlobToFile)
#define BlobToImage  PrependMagickMethod(BlobToImage)
#define BlueShiftImage  PrependMagickMethod(BlueShiftImage)
#define BlurImageChannel  PrependMagickMethod(BlurImageChannel)
#define BlurImage  PrependMagickMethod(BlurImage)
#define BorderImage  PrependMagickMethod(BorderImage)
#define CanonicalXMLContent  PrependMagickMethod(CanonicalXMLContent)
#define CatchException  PrependMagickMethod(CatchException)
#define CatchImageException  PrependMagickMethod(CatchImageException)
#define ChannelImage  PrependMagickMethod(ChannelImage)
#define ChannelThresholdImage  PrependMagickMethod(ChannelThresholdImage)
#define CharcoalImage  PrependMagickMethod(CharcoalImage)
#define ChopImage  PrependMagickMethod(ChopImage)
#define ChopPathComponents  PrependMagickMethod(ChopPathComponents)
#define ClearLinkedList  PrependMagickMethod(ClearLinkedList)
#define ClearMagickException  PrependMagickMethod(ClearMagickException)
#define ClipImagePath  PrependMagickMethod(ClipImagePath)
#define ClipImage  PrependMagickMethod(ClipImage)
#define ClipPathImage  PrependMagickMethod(ClipPathImage)
#define CloneBlobInfo  PrependMagickMethod(CloneBlobInfo)
#define CloneCacheView  PrependMagickMethod(CloneCacheView)
#define CloneDrawInfo  PrependMagickMethod(CloneDrawInfo)
#define CloneImageArtifacts  PrependMagickMethod(CloneImageArtifacts)
#define CloneImageAttributes  PrependMagickMethod(CloneImageAttributes)
#define CloneImageInfo  PrependMagickMethod(CloneImageInfo)
#define CloneImageList  PrependMagickMethod(CloneImageList)
#define CloneImageOptions  PrependMagickMethod(CloneImageOptions)
#define CloneImage  PrependMagickMethod(CloneImage)
#define CloneImageProfiles  PrependMagickMethod(CloneImageProfiles)
#define CloneImageProperties  PrependMagickMethod(CloneImageProperties)
#define CloneImages  PrependMagickMethod(CloneImages)
#define CloneMemory  PrependMagickMethod(CloneMemory)
#define CloneMontageInfo  PrependMagickMethod(CloneMontageInfo)
#define ClonePixelCacheMethods  PrependMagickMethod(ClonePixelCacheMethods)
#define CloneQuantizeInfo  PrependMagickMethod(CloneQuantizeInfo)
#define CloneSplayTree  PrependMagickMethod(CloneSplayTree)
#define CloneStringInfo  PrependMagickMethod(CloneStringInfo)
#define CloneString  PrependMagickMethod(CloneString)
#define CloseBlob  PrependMagickMethod(CloseBlob)
#define CloseCacheView  PrependMagickMethod(CloseCacheView)
#define CloseMagickLog  PrependMagickMethod(CloseMagickLog)
#define ClutImageChannel  PrependMagickMethod(ClutImageChannel)
#define ClutImage  PrependMagickMethod(ClutImage)
#define CoalesceImages  PrependMagickMethod(CoalesceImages)
#define ColorDecisionListImage  PrependMagickMethod(ColorDecisionListImage)
#define ColorFloodfillImage  PrependMagickMethod(ColorFloodfillImage)
#define ColorizeImage  PrependMagickMethod(ColorizeImage)
#define CombineImages  PrependMagickMethod(CombineImages)
#define CompareHashmapStringInfo  PrependMagickMethod(CompareHashmapStringInfo)
#define CompareHashmapString  PrependMagickMethod(CompareHashmapString)
#define CompareImageChannels  PrependMagickMethod(CompareImageChannels)
#define CompareImageLayers  PrependMagickMethod(CompareImageLayers)
#define CompareImages  PrependMagickMethod(CompareImages)
#define CompareSplayTreeStringInfo  PrependMagickMethod(CompareSplayTreeStringInfo)
#define CompareSplayTreeString  PrependMagickMethod(CompareSplayTreeString)
#define CompareStringInfo  PrependMagickMethod(CompareStringInfo)
#define CompositeImageChannel  PrependMagickMethod(CompositeImageChannel)
#define CompositeImage  PrependMagickMethod(CompositeImage)
#define CompositeLayers  PrependMagickMethod(CompositeLayers)
#define CompressImageColormap  PrependMagickMethod(CompressImageColormap)
#define ConcatenateColorComponent  PrependMagickMethod(ConcatenateColorComponent)
#define ConcatenateMagickString  PrependMagickMethod(ConcatenateMagickString)
#define ConcatenateStringInfo  PrependMagickMethod(ConcatenateStringInfo)
#define ConcatenateString  PrependMagickMethod(ConcatenateString)
#define ConfigureFileToStringInfo  PrependMagickMethod(ConfigureFileToStringInfo)
#define ConsolidateCMYKImages  PrependMagickMethod(ConsolidateCMYKImages)
#define ConstantString  PrependMagickMethod(ConstantString)
#define ConstituteImage  PrependMagickMethod(ConstituteImage)
#define ContinueTimer  PrependMagickMethod(ContinueTimer)
#define ContrastImage  PrependMagickMethod(ContrastImage)
#define ContrastStretchImageChannel  PrependMagickMethod(ContrastStretchImageChannel)
#define ContrastStretchImage  PrependMagickMethod(ContrastStretchImage)
#define ConvertHSBToRGB  PrependMagickMethod(ConvertHSBToRGB)
#define ConvertHSLToRGB  PrependMagickMethod(ConvertHSLToRGB)
#define ConvertHWBToRGB  PrependMagickMethod(ConvertHWBToRGB)
#define ConvertRGBToHSB  PrependMagickMethod(ConvertRGBToHSB)
#define ConvertRGBToHSL  PrependMagickMethod(ConvertRGBToHSL)
#define ConvertRGBToHWB  PrependMagickMethod(ConvertRGBToHWB)
#define ConvolveImageChannel  PrependMagickMethod(ConvolveImageChannel)
#define ConvolveImage  PrependMagickMethod(ConvolveImage)
#define CopyMagickMemory  PrependMagickMethod(CopyMagickMemory)
#define CopyMagickString  PrependMagickMethod(CopyMagickString)
#define CropImage  PrependMagickMethod(CropImage)
#define CycleColormapImage  PrependMagickMethod(CycleColormapImage)
#define DecipherImage  PrependMagickMethod(DecipherImage)
#define DeconstructImages  PrependMagickMethod(DeconstructImages)
#define DefineImageArtifact  PrependMagickMethod(DefineImageArtifact)
#define DefineImageOption  PrependMagickMethod(DefineImageOption)
#define DefineImageProperty  PrependMagickMethod(DefineImageProperty)
#define DefineImageRegistry  PrependMagickMethod(DefineImageRegistry)
#define DeleteImageArtifact  PrependMagickMethod(DeleteImageArtifact)
#define DeleteImageAttribute  PrependMagickMethod(DeleteImageAttribute)
#define DeleteImageFromList  PrependMagickMethod(DeleteImageFromList)
#define DeleteImageList  PrependMagickMethod(DeleteImageList)
#define DeleteImageOption  PrependMagickMethod(DeleteImageOption)
#define DeleteImageProfile  PrependMagickMethod(DeleteImageProfile)
#define DeleteImageProperty  PrependMagickMethod(DeleteImageProperty)
#define DeleteImageRegistry  PrependMagickMethod(DeleteImageRegistry)
#define DeleteImages  PrependMagickMethod(DeleteImages)
#define DeleteMagickRegistry  PrependMagickMethod(DeleteMagickRegistry)
#define DeleteNodeByValueFromSplayTree  PrependMagickMethod(DeleteNodeByValueFromSplayTree)
#define DeleteNodeFromSplayTree  PrependMagickMethod(DeleteNodeFromSplayTree)
#define DescribeImage  PrependMagickMethod(DescribeImage)
#define DeskewImage  PrependMagickMethod(DeskewImage)
#define DespeckleImage  PrependMagickMethod(DespeckleImage)
#define DestroyBlob  PrependMagickMethod(DestroyBlob)
#define DestroyCacheView  PrependMagickMethod(DestroyCacheView)
#define DestroyCoderList  PrependMagickMethod(DestroyCoderList)
#define DestroyColorList  PrependMagickMethod(DestroyColorList)
#define DestroyConfigureList  PrependMagickMethod(DestroyConfigureList)
#define DestroyConfigureOptions  PrependMagickMethod(DestroyConfigureOptions)
#define DestroyConstitute  PrependMagickMethod(DestroyConstitute)
#define DestroyDelegateList  PrependMagickMethod(DestroyDelegateList)
#define DestroyDrawInfo  PrependMagickMethod(DestroyDrawInfo)
#define DestroyExceptionInfo  PrependMagickMethod(DestroyExceptionInfo)
#define DestroyFxInfo  PrependMagickMethod(DestroyFxInfo)
#define DestroyHashmap  PrependMagickMethod(DestroyHashmap)
#define DestroyImageArtifacts  PrependMagickMethod(DestroyImageArtifacts)
#define DestroyImageAttributes  PrependMagickMethod(DestroyImageAttributes)
#define DestroyImageInfo  PrependMagickMethod(DestroyImageInfo)
#define DestroyImageList  PrependMagickMethod(DestroyImageList)
#define DestroyImageOptions  PrependMagickMethod(DestroyImageOptions)
#define DestroyImagePixels  PrependMagickMethod(DestroyImagePixels)
#define DestroyImage  PrependMagickMethod(DestroyImage)
#define DestroyImageProfiles  PrependMagickMethod(DestroyImageProfiles)
#define DestroyImageProperties  PrependMagickMethod(DestroyImageProperties)
#define DestroyImageRegistry  PrependMagickMethod(DestroyImageRegistry)
#define DestroyImages  PrependMagickMethod(DestroyImages)
#define DestroyLinkedList  PrependMagickMethod(DestroyLinkedList)
#define DestroyLocaleList  PrependMagickMethod(DestroyLocaleList)
#define DestroyLocaleOptions  PrependMagickMethod(DestroyLocaleOptions)
#define DestroyLogList  PrependMagickMethod(DestroyLogList)
#define DestroyMagickList  PrependMagickMethod(DestroyMagickList)
#define DestroyMagickMemory  PrependMagickMethod(DestroyMagickMemory)
#define DestroyMagick  PrependMagickMethod(DestroyMagick)
#define DestroyMagickRegistry  PrependMagickMethod(DestroyMagickRegistry)
#define DestroyMagickResources  PrependMagickMethod(DestroyMagickResources)
#define DestroyMagicList  PrependMagickMethod(DestroyMagicList)
#define DestroyMimeList  PrependMagickMethod(DestroyMimeList)
#define DestroyMontageInfo  PrependMagickMethod(DestroyMontageInfo)
#define DestroyPixelCacheInfo  PrependMagickMethod(DestroyPixelCacheInfo)
#define DestroyPixelCacheNexus  PrependMagickMethod(DestroyPixelCacheNexus)
#define DestroyPixelCacheResources  PrependMagickMethod(DestroyPixelCacheResources)
#define DestroyPolicyList  PrependMagickMethod(DestroyPolicyList)
#define DestroyQuantizeInfo  PrependMagickMethod(DestroyQuantizeInfo)
#define DestroyQuantumInfo  PrependMagickMethod(DestroyQuantumInfo)
#define DestroyRandomInfo  PrependMagickMethod(DestroyRandomInfo)
#define DestroyRandomReservoir  PrependMagickMethod(DestroyRandomReservoir)
#define DestroyResampleFilter  PrependMagickMethod(DestroyResampleFilter)
#define DestroyResizeFilter  PrependMagickMethod(DestroyResizeFilter)
#define DestroySemaphoreInfo  PrependMagickMethod(DestroySemaphoreInfo)
#define DestroySemaphore  PrependMagickMethod(DestroySemaphore)
#define DestroySignatureInfo  PrependMagickMethod(DestroySignatureInfo)
#define DestroySplayTree  PrependMagickMethod(DestroySplayTree)
#define DestroyStreamInfo  PrependMagickMethod(DestroyStreamInfo)
#define DestroyStringInfo  PrependMagickMethod(DestroyStringInfo)
#define DestroyStringList  PrependMagickMethod(DestroyStringList)
#define DestroyString  PrependMagickMethod(DestroyString)
#define DestroyThresholdMap  PrependMagickMethod(DestroyThresholdMap)
#define DestroyTimerInfo  PrependMagickMethod(DestroyTimerInfo)
#define DestroyTokenInfo  PrependMagickMethod(DestroyTokenInfo)
#define DestroyTypeList  PrependMagickMethod(DestroyTypeList)
#define DestroyXMLTree  PrependMagickMethod(DestroyXMLTree)
#define DestroyXResources  PrependMagickMethod(DestroyXResources)
#define DestroyXWidget  PrependMagickMethod(DestroyXWidget)
#define DetachBlob  PrependMagickMethod(DetachBlob)
#define DisassociateImageStream  PrependMagickMethod(DisassociateImageStream)
#define DispatchImage  PrependMagickMethod(DispatchImage)
#define DisplayImages  PrependMagickMethod(DisplayImages)
#define DisposeImages  PrependMagickMethod(DisposeImages)
#define DistortImage  PrependMagickMethod(DistortImage)
#define DrawAffineImage  PrependMagickMethod(DrawAffineImage)
#define DrawClipPath  PrependMagickMethod(DrawClipPath)
#define DrawGradientImage  PrependMagickMethod(DrawGradientImage)
#define DrawImage  PrependMagickMethod(DrawImage)
#define DrawPatternPath  PrependMagickMethod(DrawPatternPath)
#define DrawPrimitive  PrependMagickMethod(DrawPrimitive)
#define DuplicateBlob  PrependMagickMethod(DuplicateBlob)
#define EdgeImage  PrependMagickMethod(EdgeImage)
#define EmbossImage  PrependMagickMethod(EmbossImage)
#define EncipherImage  PrependMagickMethod(EncipherImage)
#define EnhanceImage  PrependMagickMethod(EnhanceImage)
#define EOFBlob  PrependMagickMethod(EOFBlob)
#define EqualizeImageChannel  PrependMagickMethod(EqualizeImageChannel)
#define EqualizeImage  PrependMagickMethod(EqualizeImage)
#define EscapeString  PrependMagickMethod(EscapeString)
#define EvaluateImageChannel  PrependMagickMethod(EvaluateImageChannel)
#define EvaluateImage  PrependMagickMethod(EvaluateImage)
#define ExcerptImage  PrependMagickMethod(ExcerptImage)
#define ExpandAffine  PrependMagickMethod(ExpandAffine)
#define ExpandFilename  PrependMagickMethod(ExpandFilename)
#define ExpandFilenames  PrependMagickMethod(ExpandFilenames)
#define ExportImagePixels  PrependMagickMethod(ExportImagePixels)
#define ExportQuantumPixels  PrependMagickMethod(ExportQuantumPixels)
#define ExtentImage  PrependMagickMethod(ExtentImage)
#define ExtractSubimageFromImage  PrependMagickMethod(ExtractSubimageFromImage)
#define FileToBlob  PrependMagickMethod(FileToBlob)
#define FileToImage  PrependMagickMethod(FileToImage)
#define FileToStringInfo  PrependMagickMethod(FileToStringInfo)
#define FileToString  PrependMagickMethod(FileToString)
#define FinalizeSignature  PrependMagickMethod(FinalizeSignature)
#define FlattenImages  PrependMagickMethod(FlattenImages)
#define FlipImage  PrependMagickMethod(FlipImage)
#define FloodfillPaintImage  PrependMagickMethod(FloodfillPaintImage)
#define FlopImage  PrependMagickMethod(FlopImage)
#define FormatImageAttributeList  PrependMagickMethod(FormatImageAttributeList)
#define FormatImageAttribute  PrependMagickMethod(FormatImageAttribute)
#define FormatImagePropertyList  PrependMagickMethod(FormatImagePropertyList)
#define FormatImageProperty  PrependMagickMethod(FormatImageProperty)
#define FormatMagickCaption  PrependMagickMethod(FormatMagickCaption)
#define FormatMagickSize  PrependMagickMethod(FormatMagickSize)
#define FormatMagickStringList  PrependMagickMethod(FormatMagickStringList)
#define FormatMagickString  PrependMagickMethod(FormatMagickString)
#define FormatMagickTime  PrependMagickMethod(FormatMagickTime)
#define FormatStringList  PrependMagickMethod(FormatStringList)
#define FormatString  PrependMagickMethod(FormatString)
#define ForwardFourierTransformImage  PrependMagickMethod(ForwardFourierTransformImage)
#define FrameImage  PrependMagickMethod(FrameImage)
#define FunctionImageChannel  PrependMagickMethod(FunctionImageChannel)
#define FunctionImage  PrependMagickMethod(FunctionImage)
#define FuzzyColorCompare  PrependMagickMethod(FuzzyColorCompare)
#define FuzzyColorMatch  PrependMagickMethod(FuzzyColorMatch)
#define FuzzyOpacityCompare  PrependMagickMethod(FuzzyOpacityCompare)
#define FxEvaluateChannelExpression  PrependMagickMethod(FxEvaluateChannelExpression)
#define FxEvaluateExpression  PrependMagickMethod(FxEvaluateExpression)
#define FxImageChannel  PrependMagickMethod(FxImageChannel)
#define FxImage  PrependMagickMethod(FxImage)
#define FxPreprocessExpression  PrependMagickMethod(FxPreprocessExpression)
#define GammaImageChannel  PrependMagickMethod(GammaImageChannel)
#define GammaImage  PrependMagickMethod(GammaImage)
#define GaussianBlurImageChannel  PrependMagickMethod(GaussianBlurImageChannel)
#define GaussianBlurImage  PrependMagickMethod(GaussianBlurImage)
#define GaussJordanElimination  PrependMagickMethod(GaussJordanElimination)
#define GenerateDifferentialNoise  PrependMagickMethod(GenerateDifferentialNoise)
#define GetAffineMatrix  PrependMagickMethod(GetAffineMatrix)
#define GetAuthenticIndexQueue  PrependMagickMethod(GetAuthenticIndexQueue)
#define GetAuthenticPixelCacheNexus  PrependMagickMethod(GetAuthenticPixelCacheNexus)
#define GetAuthenticPixelQueue  PrependMagickMethod(GetAuthenticPixelQueue)
#define GetAuthenticPixels  PrependMagickMethod(GetAuthenticPixels)
#define GetBlobError  PrependMagickMethod(GetBlobError)
#define GetBlobFileHandle  PrependMagickMethod(GetBlobFileHandle)
#define GetBlobInfo  PrependMagickMethod(GetBlobInfo)
#define GetBlobProperties  PrependMagickMethod(GetBlobProperties)
#define GetBlobSize  PrependMagickMethod(GetBlobSize)
#define GetBlobStreamData  PrependMagickMethod(GetBlobStreamData)
#define GetBlobStreamHandler  PrependMagickMethod(GetBlobStreamHandler)
#define GetCacheViewAuthenticIndexQueue  PrependMagickMethod(GetCacheViewAuthenticIndexQueue)
#define GetCacheViewAuthenticPixelQueue  PrependMagickMethod(GetCacheViewAuthenticPixelQueue)
#define GetCacheViewAuthenticPixels  PrependMagickMethod(GetCacheViewAuthenticPixels)
#define GetCacheViewColorspace  PrependMagickMethod(GetCacheViewColorspace)
#define GetCacheViewException  PrependMagickMethod(GetCacheViewException)
#define GetCacheViewExtent  PrependMagickMethod(GetCacheViewExtent)
#define GetCacheViewIndexes  PrependMagickMethod(GetCacheViewIndexes)
#define GetCacheViewPixels  PrependMagickMethod(GetCacheViewPixels)
#define GetCacheView  PrependMagickMethod(GetCacheView)
#define GetCacheViewStorageClass  PrependMagickMethod(GetCacheViewStorageClass)
#define GetCacheViewVirtualIndexQueue  PrependMagickMethod(GetCacheViewVirtualIndexQueue)
#define GetCacheViewVirtualPixelQueue  PrependMagickMethod(GetCacheViewVirtualPixelQueue)
#define GetCacheViewVirtualPixels  PrependMagickMethod(GetCacheViewVirtualPixels)
#define GetClientName  PrependMagickMethod(GetClientName)
#define GetClientPath  PrependMagickMethod(GetClientPath)
#define GetCoderInfoList  PrependMagickMethod(GetCoderInfoList)
#define GetCoderInfo  PrependMagickMethod(GetCoderInfo)
#define GetCoderList  PrependMagickMethod(GetCoderList)
#define GetColorInfoList  PrependMagickMethod(GetColorInfoList)
#define GetColorInfo  PrependMagickMethod(GetColorInfo)
#define GetColorList  PrependMagickMethod(GetColorList)
#define GetColorTuple  PrependMagickMethod(GetColorTuple)
#define GetConfigureBlob  PrependMagickMethod(GetConfigureBlob)
#define GetConfigureInfoList  PrependMagickMethod(GetConfigureInfoList)
#define GetConfigureInfo  PrependMagickMethod(GetConfigureInfo)
#define GetConfigureList  PrependMagickMethod(GetConfigureList)
#define GetConfigureOption  PrependMagickMethod(GetConfigureOption)
#define GetConfigureOptions  PrependMagickMethod(GetConfigureOptions)
#define GetConfigurePaths  PrependMagickMethod(GetConfigurePaths)
#define GetConfigureValue  PrependMagickMethod(GetConfigureValue)
#define GetDelegateCommand  PrependMagickMethod(GetDelegateCommand)
#define GetDelegateCommands  PrependMagickMethod(GetDelegateCommands)
#define GetDelegateInfoList  PrependMagickMethod(GetDelegateInfoList)
#define GetDelegateInfo  PrependMagickMethod(GetDelegateInfo)
#define GetDelegateList  PrependMagickMethod(GetDelegateList)
#define GetDelegateMode  PrependMagickMethod(GetDelegateMode)
#define GetDelegateThreadSupport  PrependMagickMethod(GetDelegateThreadSupport)
#define GetDrawInfo  PrependMagickMethod(GetDrawInfo)
#define GetElapsedTime  PrependMagickMethod(GetElapsedTime)
#define GetEnvironmentValue  PrependMagickMethod(GetEnvironmentValue)
#define GetExceptionInfo  PrependMagickMethod(GetExceptionInfo)
#define GetExceptionMessage  PrependMagickMethod(GetExceptionMessage)
#define GetExecutionPath  PrependMagickMethod(GetExecutionPath)
#define GetFirstImageInList  PrependMagickMethod(GetFirstImageInList)
#define GetGeometry  PrependMagickMethod(GetGeometry)
#define GetImageAlphaChannel  PrependMagickMethod(GetImageAlphaChannel)
#define GetImageArtifact  PrependMagickMethod(GetImageArtifact)
#define GetImageAttribute  PrependMagickMethod(GetImageAttribute)
#define GetImageBoundingBox  PrependMagickMethod(GetImageBoundingBox)
#define GetImageChannelDepth  PrependMagickMethod(GetImageChannelDepth)
#define GetImageChannelDistortion  PrependMagickMethod(GetImageChannelDistortion)
#define GetImageChannelDistortions  PrependMagickMethod(GetImageChannelDistortions)
#define GetImageChannelExtrema  PrependMagickMethod(GetImageChannelExtrema)
#define GetImageChannelKurtosis  PrependMagickMethod(GetImageChannelKurtosis)
#define GetImageChannelMean  PrependMagickMethod(GetImageChannelMean)
#define GetImageChannelRange  PrependMagickMethod(GetImageChannelRange)
#define GetImageChannelStatistics  PrependMagickMethod(GetImageChannelStatistics)
#define GetImageClipMask  PrependMagickMethod(GetImageClipMask)
#define GetImageClippingPathAttribute  PrependMagickMethod(GetImageClippingPathAttribute)
#define GetImageDecoder  PrependMagickMethod(GetImageDecoder)
#define GetImageDepth  PrependMagickMethod(GetImageDepth)
#define GetImageDistortion  PrependMagickMethod(GetImageDistortion)
#define GetImageDynamicThreshold  PrependMagickMethod(GetImageDynamicThreshold)
#define GetImageEncoder  PrependMagickMethod(GetImageEncoder)
#define GetImageException  PrependMagickMethod(GetImageException)
#define GetImageExtent  PrependMagickMethod(GetImageExtent)
#define GetImageExtrema  PrependMagickMethod(GetImageExtrema)
#define GetImageFromList  PrependMagickMethod(GetImageFromList)
#define GetImageFromMagickRegistry  PrependMagickMethod(GetImageFromMagickRegistry)
#define GetImageGeometry  PrependMagickMethod(GetImageGeometry)
#define GetImageHistogram  PrependMagickMethod(GetImageHistogram)
#define GetImageIndexInList  PrependMagickMethod(GetImageIndexInList)
#define GetImageInfo  PrependMagickMethod(GetImageInfo)
#define GetImageKurtosis  PrependMagickMethod(GetImageKurtosis)
#define GetImageListIndex  PrependMagickMethod(GetImageListIndex)
#define GetImageListLength  PrependMagickMethod(GetImageListLength)
#define GetImageList  PrependMagickMethod(GetImageList)
#define GetImageListSize  PrependMagickMethod(GetImageListSize)
#define GetImageMagick  PrependMagickMethod(GetImageMagick)
#define GetImageMask  PrependMagickMethod(GetImageMask)
#define GetImageMean  PrependMagickMethod(GetImageMean)
#define GetImageOption  PrependMagickMethod(GetImageOption)
#define GetImagePixelCache  PrependMagickMethod(GetImagePixelCache)
#define GetImagePixels  PrependMagickMethod(GetImagePixels)
#define GetImageProfile  PrependMagickMethod(GetImageProfile)
#define GetImageProperty  PrependMagickMethod(GetImageProperty)
#define GetImageQuantizeError  PrependMagickMethod(GetImageQuantizeError)
#define GetImageQuantumDepth  PrependMagickMethod(GetImageQuantumDepth)
#define GetImageRange  PrependMagickMethod(GetImageRange)
#define GetImageRegistry  PrependMagickMethod(GetImageRegistry)
#define GetImageTotalInkDensity  PrependMagickMethod(GetImageTotalInkDensity)
#define GetImageType  PrependMagickMethod(GetImageType)
#define GetImageVirtualPixelMethod  PrependMagickMethod(GetImageVirtualPixelMethod)
#define GetIndexes  PrependMagickMethod(GetIndexes)
#define GetLastImageInList  PrependMagickMethod(GetLastImageInList)
#define GetLastValueInLinkedList  PrependMagickMethod(GetLastValueInLinkedList)
#define GetLocaleExceptionMessage  PrependMagickMethod(GetLocaleExceptionMessage)
#define GetLocaleInfoList  PrependMagickMethod(GetLocaleInfoList)
#define GetLocaleInfo_  PrependMagickMethod(GetLocaleInfo_)
#define GetLocaleList  PrependMagickMethod(GetLocaleList)
#define GetLocaleMessage  PrependMagickMethod(GetLocaleMessage)
#define GetLocaleOptions  PrependMagickMethod(GetLocaleOptions)
#define GetLocaleValue  PrependMagickMethod(GetLocaleValue)
#define GetLogInfoList  PrependMagickMethod(GetLogInfoList)
#define GetLogList  PrependMagickMethod(GetLogList)
#define GetLogName  PrependMagickMethod(GetLogName)
#define GetMagicInfoList  PrependMagickMethod(GetMagicInfoList)
#define GetMagicInfo  PrependMagickMethod(GetMagicInfo)
#define GetMagickAdjoin  PrependMagickMethod(GetMagickAdjoin)
#define GetMagickBlobSupport  PrependMagickMethod(GetMagickBlobSupport)
#define GetMagickCopyright  PrependMagickMethod(GetMagickCopyright)
#define GetMagickDescription  PrependMagickMethod(GetMagickDescription)
#define GetMagickEndianSupport  PrependMagickMethod(GetMagickEndianSupport)
#define GetMagickGeometry  PrependMagickMethod(GetMagickGeometry)
#define GetMagickHomeURL  PrependMagickMethod(GetMagickHomeURL)
#define GetMagickInfoList  PrependMagickMethod(GetMagickInfoList)
#define GetMagickInfo  PrependMagickMethod(GetMagickInfo)
#define GetMagickList  PrependMagickMethod(GetMagickList)
#define GetMagickMemoryMethods  PrependMagickMethod(GetMagickMemoryMethods)
#define GetMagickOptions  PrependMagickMethod(GetMagickOptions)
#define GetMagickPackageName  PrependMagickMethod(GetMagickPackageName)
#define GetMagickPixelPacket  PrependMagickMethod(GetMagickPixelPacket)
#define GetMagickProperty  PrependMagickMethod(GetMagickProperty)
#define GetMagickQuantumDepth  PrependMagickMethod(GetMagickQuantumDepth)
#define GetMagickQuantumRange  PrependMagickMethod(GetMagickQuantumRange)
#define GetMagickRawSupport  PrependMagickMethod(GetMagickRawSupport)
#define GetMagickRegistry  PrependMagickMethod(GetMagickRegistry)
#define GetMagickReleaseDate  PrependMagickMethod(GetMagickReleaseDate)
#define GetMagickResourceLimit  PrependMagickMethod(GetMagickResourceLimit)
#define GetMagickResource  PrependMagickMethod(GetMagickResource)
#define GetMagickSeekableStream  PrependMagickMethod(GetMagickSeekableStream)
#define GetMagickThreadSupport  PrependMagickMethod(GetMagickThreadSupport)
#define GetMagickToken  PrependMagickMethod(GetMagickToken)
#define GetMagickVersion  PrependMagickMethod(GetMagickVersion)
#define GetMagicList  PrependMagickMethod(GetMagicList)
#define GetMagicName  PrependMagickMethod(GetMagicName)
#define GetMimeDescription  PrependMagickMethod(GetMimeDescription)
#define GetMimeInfoList  PrependMagickMethod(GetMimeInfoList)
#define GetMimeInfo  PrependMagickMethod(GetMimeInfo)
#define GetMimeList  PrependMagickMethod(GetMimeList)
#define GetMimeType  PrependMagickMethod(GetMimeType)
#define GetMonitorHandler  PrependMagickMethod(GetMonitorHandler)
#define GetMontageInfo  PrependMagickMethod(GetMontageInfo)
#define GetMultilineTypeMetrics  PrependMagickMethod(GetMultilineTypeMetrics)
#define GetNextImageArtifact  PrependMagickMethod(GetNextImageArtifact)
#define GetNextImageAttribute  PrependMagickMethod(GetNextImageAttribute)
#define GetNextImageInList  PrependMagickMethod(GetNextImageInList)
#define GetNextImageOption  PrependMagickMethod(GetNextImageOption)
#define GetNextImage  PrependMagickMethod(GetNextImage)
#define GetNextImageProfile  PrependMagickMethod(GetNextImageProfile)
#define GetNextImageProperty  PrependMagickMethod(GetNextImageProperty)
#define GetNextImageRegistry  PrependMagickMethod(GetNextImageRegistry)
#define GetNextKeyInHashmap  PrependMagickMethod(GetNextKeyInHashmap)
#define GetNextKeyInSplayTree  PrependMagickMethod(GetNextKeyInSplayTree)
#define GetNextValueInHashmap  PrependMagickMethod(GetNextValueInHashmap)
#define GetNextValueInLinkedList  PrependMagickMethod(GetNextValueInLinkedList)
#define GetNextValueInSplayTree  PrependMagickMethod(GetNextValueInSplayTree)
#define GetNextXMLTreeTag  PrependMagickMethod(GetNextXMLTreeTag)
#define GetNumberColors  PrependMagickMethod(GetNumberColors)
#define GetNumberOfElementsInLinkedList  PrependMagickMethod(GetNumberOfElementsInLinkedList)
#define GetNumberOfEntriesInHashmap  PrependMagickMethod(GetNumberOfEntriesInHashmap)
#define GetNumberOfNodesInSplayTree  PrependMagickMethod(GetNumberOfNodesInSplayTree)
#define GetNumberScenes  PrependMagickMethod(GetNumberScenes)
#define GetOneAuthenticPixel  PrependMagickMethod(GetOneAuthenticPixel)
#define GetOneCacheViewAuthenticPixel  PrependMagickMethod(GetOneCacheViewAuthenticPixel)
#define GetOneCacheViewVirtualMethodPixel  PrependMagickMethod(GetOneCacheViewVirtualMethodPixel)
#define GetOneCacheViewVirtualPixel  PrependMagickMethod(GetOneCacheViewVirtualPixel)
#define GetOnePixel  PrependMagickMethod(GetOnePixel)
#define GetOneVirtualMagickPixel  PrependMagickMethod(GetOneVirtualMagickPixel)
#define GetOneVirtualMethodPixel  PrependMagickMethod(GetOneVirtualMethodPixel)
#define GetOneVirtualPixel  PrependMagickMethod(GetOneVirtualPixel)
#define GetOptimalKernelWidth1D  PrependMagickMethod(GetOptimalKernelWidth1D)
#define GetOptimalKernelWidth2D  PrependMagickMethod(GetOptimalKernelWidth2D)
#define GetOptimalKernelWidth  PrependMagickMethod(GetOptimalKernelWidth)
#define GetPageGeometry  PrependMagickMethod(GetPageGeometry)
#define GetPathAttributes  PrependMagickMethod(GetPathAttributes)
#define GetPathComponent  PrependMagickMethod(GetPathComponent)
#define GetPathComponents  PrependMagickMethod(GetPathComponents)
#define GetPixelCacheColorspace  PrependMagickMethod(GetPixelCacheColorspace)
#define GetPixelCacheMethods  PrependMagickMethod(GetPixelCacheMethods)
#define GetPixelCacheNexusExtent  PrependMagickMethod(GetPixelCacheNexusExtent)
#define GetPixelCacheNexusIndexes  PrependMagickMethod(GetPixelCacheNexusIndexes)
#define GetPixelCacheNexusPixels  PrependMagickMethod(GetPixelCacheNexusPixels)
#define GetPixelCacheStorageClass  PrependMagickMethod(GetPixelCacheStorageClass)
#define GetPixelCacheVirtualMethod  PrependMagickMethod(GetPixelCacheVirtualMethod)
#define GetPixels  PrependMagickMethod(GetPixels)
#define GetPolicyInfoList  PrependMagickMethod(GetPolicyInfoList)
#define GetPolicyList  PrependMagickMethod(GetPolicyList)
#define GetPolicyValue  PrependMagickMethod(GetPolicyValue)
#define GetPreviousImageInList  PrependMagickMethod(GetPreviousImageInList)
#define GetPreviousImage  PrependMagickMethod(GetPreviousImage)
#define GetPseudoRandomValue  PrependMagickMethod(GetPseudoRandomValue)
#define GetQuantizeInfo  PrependMagickMethod(GetQuantizeInfo)
#define GetQuantumExtent  PrependMagickMethod(GetQuantumExtent)
#define GetQuantumInfo  PrependMagickMethod(GetQuantumInfo)
#define GetQuantumPixels  PrependMagickMethod(GetQuantumPixels)
#define GetQuantumType  PrependMagickMethod(GetQuantumType)
#define GetRandomKey  PrependMagickMethod(GetRandomKey)
#define GetRandomValue  PrependMagickMethod(GetRandomValue)
#define GetResizeFilterSupport  PrependMagickMethod(GetResizeFilterSupport)
#define GetResizeFilterWeight  PrependMagickMethod(GetResizeFilterWeight)
#define GetSignatureBlocksize  PrependMagickMethod(GetSignatureBlocksize)
#define GetSignatureDigest  PrependMagickMethod(GetSignatureDigest)
#define GetSignatureDigestsize  PrependMagickMethod(GetSignatureDigestsize)
#define GetStreamInfoClientData  PrependMagickMethod(GetStreamInfoClientData)
#define GetStringInfoDatum  PrependMagickMethod(GetStringInfoDatum)
#define GetStringInfoLength  PrependMagickMethod(GetStringInfoLength)
#define GetStringInfoPath  PrependMagickMethod(GetStringInfoPath)
#define GetThresholdMapFile  PrependMagickMethod(GetThresholdMapFile)
#define GetThresholdMap  PrependMagickMethod(GetThresholdMap)
#define GetTimerInfo  PrependMagickMethod(GetTimerInfo)
#define GetTypeInfoByFamily  PrependMagickMethod(GetTypeInfoByFamily)
#define GetTypeInfoList  PrependMagickMethod(GetTypeInfoList)
#define GetTypeInfo  PrependMagickMethod(GetTypeInfo)
#define GetTypeList  PrependMagickMethod(GetTypeList)
#define GetTypeMetrics  PrependMagickMethod(GetTypeMetrics)
#define GetUserTime  PrependMagickMethod(GetUserTime)
#define GetValueFromHashmap  PrependMagickMethod(GetValueFromHashmap)
#define GetValueFromLinkedList  PrependMagickMethod(GetValueFromLinkedList)
#define GetValueFromSplayTree  PrependMagickMethod(GetValueFromSplayTree)
#define GetVirtualIndexesFromNexus  PrependMagickMethod(GetVirtualIndexesFromNexus)
#define GetVirtualIndexQueue  PrependMagickMethod(GetVirtualIndexQueue)
#define GetVirtualPixelQueue  PrependMagickMethod(GetVirtualPixelQueue)
#define GetVirtualPixelsFromNexus  PrependMagickMethod(GetVirtualPixelsFromNexus)
#define GetVirtualPixelsNexus  PrependMagickMethod(GetVirtualPixelsNexus)
#define GetVirtualPixels  PrependMagickMethod(GetVirtualPixels)
#define GetXMLTreeAttribute  PrependMagickMethod(GetXMLTreeAttribute)
#define GetXMLTreeAttributes  PrependMagickMethod(GetXMLTreeAttributes)
#define GetXMLTreeChild  PrependMagickMethod(GetXMLTreeChild)
#define GetXMLTreeContent  PrependMagickMethod(GetXMLTreeContent)
#define GetXMLTreeOrdered  PrependMagickMethod(GetXMLTreeOrdered)
#define GetXMLTreePath  PrependMagickMethod(GetXMLTreePath)
#define GetXMLTreeProcessingInstructions  PrependMagickMethod(GetXMLTreeProcessingInstructions)
#define GetXMLTreeSibling  PrependMagickMethod(GetXMLTreeSibling)
#define GetXMLTreeTag  PrependMagickMethod(GetXMLTreeTag)
#define GlobExpression  PrependMagickMethod(GlobExpression)
#define GradientImage  PrependMagickMethod(GradientImage)
#define GravityAdjustGeometry  PrependMagickMethod(GravityAdjustGeometry)
#define HaldClutImageChannel  PrependMagickMethod(HaldClutImageChannel)
#define HaldClutImage  PrependMagickMethod(HaldClutImage)
#define HashPointerType  PrependMagickMethod(HashPointerType)
#define HashStringInfoType  PrependMagickMethod(HashStringInfoType)
#define HashStringType  PrependMagickMethod(HashStringType)
#define HSLTransform  PrependMagickMethod(HSLTransform)
#define Huffman2DEncodeImage  PrependMagickMethod(Huffman2DEncodeImage)
#define HuffmanDecodeImage  PrependMagickMethod(HuffmanDecodeImage)
#define HuffmanEncodeImage  PrependMagickMethod(HuffmanEncodeImage)
#define IdentifyImage  PrependMagickMethod(IdentifyImage)
#define IdentityAffine  PrependMagickMethod(IdentityAffine)
#define ImageListToArray  PrependMagickMethod(ImageListToArray)
#define ImagesToBlob  PrependMagickMethod(ImagesToBlob)
#define ImageToBlob  PrependMagickMethod(ImageToBlob)
#define ImageToFile  PrependMagickMethod(ImageToFile)
#define ImplodeImage  PrependMagickMethod(ImplodeImage)
#define ImportImagePixels  PrependMagickMethod(ImportImagePixels)
#define ImportQuantumPixels  PrependMagickMethod(ImportQuantumPixels)
#define increase  PrependMagickMethod(increase)
#define InheritException  PrependMagickMethod(InheritException)
#define InitializeMagick  PrependMagickMethod(InitializeMagick)
#define InitializeMagickResources  PrependMagickMethod(InitializeMagickResources)
#define InitializeSemaphore  PrependMagickMethod(InitializeSemaphore)
#define InitializeSignature  PrependMagickMethod(InitializeSignature)
#define InjectImageBlob  PrependMagickMethod(InjectImageBlob)
#define InsertImageInList  PrependMagickMethod(InsertImageInList)
#define InsertTagIntoXMLTree  PrependMagickMethod(InsertTagIntoXMLTree)
#define InsertValueInLinkedList  PrependMagickMethod(InsertValueInLinkedList)
#define InsertValueInSortedLinkedList  PrependMagickMethod(InsertValueInSortedLinkedList)
#define InterpolatePixelColor  PrependMagickMethod(InterpolatePixelColor)
#define InterpretImageAttributes  PrependMagickMethod(InterpretImageAttributes)
#define InterpretImageFilename  PrependMagickMethod(InterpretImageFilename)
#define InterpretImageProperties  PrependMagickMethod(InterpretImageProperties)
#define InverseFourierTransformImage  PrependMagickMethod(InverseFourierTransformImage)
#define InvokeDelegate  PrependMagickMethod(InvokeDelegate)
#define InvokeDynamicImageFilter  PrependMagickMethod(InvokeDynamicImageFilter)
#define IsBlobExempt  PrependMagickMethod(IsBlobExempt)
#define IsBlobSeekable  PrependMagickMethod(IsBlobSeekable)
#define IsBlobTemporary  PrependMagickMethod(IsBlobTemporary)
#define IsColorSimilar  PrependMagickMethod(IsColorSimilar)
#define IsEventLogging  PrependMagickMethod(IsEventLogging)
#define IsGeometry  PrependMagickMethod(IsGeometry)
#define IsGlob  PrependMagickMethod(IsGlob)
#define IsGrayImage  PrependMagickMethod(IsGrayImage)
#define IsHashmapEmpty  PrependMagickMethod(IsHashmapEmpty)
#define IsHighDynamicRangeImage  PrependMagickMethod(IsHighDynamicRangeImage)
#define IsHistogramImage  PrependMagickMethod(IsHistogramImage)
#define IsImageObject  PrependMagickMethod(IsImageObject)
#define IsImagesEqual  PrependMagickMethod(IsImagesEqual)
#define IsImageSimilar  PrependMagickMethod(IsImageSimilar)
#define IsLinkedListEmpty  PrependMagickMethod(IsLinkedListEmpty)
#define IsMagickColorSimilar  PrependMagickMethod(IsMagickColorSimilar)
#define IsMagickConflict  PrependMagickMethod(IsMagickConflict)
#define IsMagickInstantiated  PrependMagickMethod(IsMagickInstantiated)
#define IsMagickOption  PrependMagickMethod(IsMagickOption)
#define IsMagickTrue  PrependMagickMethod(IsMagickTrue)
#define IsMonochromeImage  PrependMagickMethod(IsMonochromeImage)
#define IsOpacitySimilar  PrependMagickMethod(IsOpacitySimilar)
#define IsOpaqueImage  PrependMagickMethod(IsOpaqueImage)
#define IsPaletteImage  PrependMagickMethod(IsPaletteImage)
#define IsPathAccessible  PrependMagickMethod(IsPathAccessible)
#define IsRightsAuthorized  PrependMagickMethod(IsRightsAuthorized)
#define IsSceneGeometry  PrependMagickMethod(IsSceneGeometry)
#define IsSubimage  PrependMagickMethod(IsSubimage)
#define IsTaintImage  PrependMagickMethod(IsTaintImage)
#define LeastSquaresAddTerms  PrependMagickMethod(LeastSquaresAddTerms)
#define LevelImageChannel  PrependMagickMethod(LevelImageChannel)
#define LevelImageColors  PrependMagickMethod(LevelImageColors)
#define LevelImage  PrependMagickMethod(LevelImage)
#define LevelizeImageChannel  PrependMagickMethod(LevelizeImageChannel)
#define LiberateMemory  PrependMagickMethod(LiberateMemory)
#define LiberateSemaphoreInfo  PrependMagickMethod(LiberateSemaphoreInfo)
#define LinearStretchImage  PrependMagickMethod(LinearStretchImage)
#define LinkedListToArray  PrependMagickMethod(LinkedListToArray)
#define LiquidRescaleImage  PrependMagickMethod(LiquidRescaleImage)
#define ListCoderInfo  PrependMagickMethod(ListCoderInfo)
#define ListColorInfo  PrependMagickMethod(ListColorInfo)
#define ListConfigureInfo  PrependMagickMethod(ListConfigureInfo)
#define ListDelegateInfo  PrependMagickMethod(ListDelegateInfo)
#define ListFiles  PrependMagickMethod(ListFiles)
#define ListLocaleInfo  PrependMagickMethod(ListLocaleInfo)
#define ListLogInfo  PrependMagickMethod(ListLogInfo)
#define ListMagicInfo  PrependMagickMethod(ListMagicInfo)
#define ListMagickInfo  PrependMagickMethod(ListMagickInfo)
#define ListMagickOptions  PrependMagickMethod(ListMagickOptions)
#define ListMagickResourceInfo  PrependMagickMethod(ListMagickResourceInfo)
#define ListMimeInfo  PrependMagickMethod(ListMimeInfo)
#define ListModuleInfo  PrependMagickMethod(ListModuleInfo)
#define ListPolicyInfo  PrependMagickMethod(ListPolicyInfo)
#define ListThresholdMapFile  PrependMagickMethod(ListThresholdMapFile)
#define ListThresholdMaps  PrependMagickMethod(ListThresholdMaps)
#define ListTypeInfo  PrependMagickMethod(ListTypeInfo)
#define LoadFontConfigFonts  PrependMagickMethod(LoadFontConfigFonts)
#define LoadMimeLists  PrependMagickMethod(LoadMimeLists)
#define LocaleCompare  PrependMagickMethod(LocaleCompare)
#define LocaleLower  PrependMagickMethod(LocaleLower)
#define LocaleNCompare  PrependMagickMethod(LocaleNCompare)
#define LocaleUpper  PrependMagickMethod(LocaleUpper)
#define LockSemaphoreInfo  PrependMagickMethod(LockSemaphoreInfo)
#define LogMagickEventList  PrependMagickMethod(LogMagickEventList)
#define LogMagickEvent  PrependMagickMethod(LogMagickEvent)
#define LZWEncodeImage  PrependMagickMethod(LZWEncodeImage)
#define MagickCoreGenesis  PrependMagickMethod(MagickCoreGenesis)
#define MagickCoreTerminus  PrependMagickMethod(MagickCoreTerminus)
#define MagickCreateThreadKey  PrependMagickMethod(MagickCreateThreadKey)
#define MagickDeleteThreadKey  PrependMagickMethod(MagickDeleteThreadKey)
#define MagickError  PrependMagickMethod(MagickError)
#define MagickFatalError  PrependMagickMethod(MagickFatalError)
#define MagickGetThreadValue  PrependMagickMethod(MagickGetThreadValue)
#define MagickIncarnate  PrependMagickMethod(MagickIncarnate)
#define MagickMonitor  PrependMagickMethod(MagickMonitor)
#define MagickOptionToMnemonic  PrependMagickMethod(MagickOptionToMnemonic)
#define MagickSetThreadValue  PrependMagickMethod(MagickSetThreadValue)
#define MagickToMime  PrependMagickMethod(MagickToMime)
#define MagickWarning  PrependMagickMethod(MagickWarning)
#define MagnifyImage  PrependMagickMethod(MagnifyImage)
#define MapBlob  PrependMagickMethod(MapBlob)
#define MapImage  PrependMagickMethod(MapImage)
#define MapImages  PrependMagickMethod(MapImages)
#define MatteFloodfillImage  PrependMagickMethod(MatteFloodfillImage)
#define MedianFilterImage  PrependMagickMethod(MedianFilterImage)
#define MergeImageLayers  PrependMagickMethod(MergeImageLayers)
#define MinifyImage  PrependMagickMethod(MinifyImage)
#define ModifyImage  PrependMagickMethod(ModifyImage)
#define ModulateImage  PrependMagickMethod(ModulateImage)
#define MontageImageList  PrependMagickMethod(MontageImageList)
#define MontageImages  PrependMagickMethod(MontageImages)
#define MorphImages  PrependMagickMethod(MorphImages)
#define MosaicImages  PrependMagickMethod(MosaicImages)
#define MotionBlurImageChannel  PrependMagickMethod(MotionBlurImageChannel)
#define MotionBlurImage  PrependMagickMethod(MotionBlurImage)
#define MSBOrderLong  PrependMagickMethod(MSBOrderLong)
#define MSBOrderShort  PrependMagickMethod(MSBOrderShort)
#define MultilineCensus  PrependMagickMethod(MultilineCensus)
#define NegateImageChannel  PrependMagickMethod(NegateImageChannel)
#define NegateImage  PrependMagickMethod(NegateImage)
#define NewHashmap  PrependMagickMethod(NewHashmap)
#define NewImageList  PrependMagickMethod(NewImageList)
#define NewLinkedList  PrependMagickMethod(NewLinkedList)
#define NewMagickImage  PrependMagickMethod(NewMagickImage)
#define NewSplayTree  PrependMagickMethod(NewSplayTree)
#define NewXMLTree  PrependMagickMethod(NewXMLTree)
#define NewXMLTreeTag  PrependMagickMethod(NewXMLTreeTag)
#define NormalizeImageChannel  PrependMagickMethod(NormalizeImageChannel)
#define NormalizeImage  PrependMagickMethod(NormalizeImage)
#define OilPaintImage  PrependMagickMethod(OilPaintImage)
#define OpaqueImage  PrependMagickMethod(OpaqueImage)
#define OpaquePaintImageChannel  PrependMagickMethod(OpaquePaintImageChannel)
#define OpaquePaintImage  PrependMagickMethod(OpaquePaintImage)
#define OpenBlob  PrependMagickMethod(OpenBlob)
#define OpenCacheView  PrependMagickMethod(OpenCacheView)
#define OpenMagickStream  PrependMagickMethod(OpenMagickStream)
#define OpenStream  PrependMagickMethod(OpenStream)
#define OptimizeImageLayers  PrependMagickMethod(OptimizeImageLayers)
#define OptimizeImageTransparency  PrependMagickMethod(OptimizeImageTransparency)
#define OptimizePlusImageLayers  PrependMagickMethod(OptimizePlusImageLayers)
#define OrderedDitherImageChannel  PrependMagickMethod(OrderedDitherImageChannel)
#define OrderedDitherImage  PrependMagickMethod(OrderedDitherImage)
#define OrderedPosterizeImageChannel  PrependMagickMethod(OrderedPosterizeImageChannel)
#define OrderedPosterizeImage  PrependMagickMethod(OrderedPosterizeImage)
#define PackbitsEncodeImage  PrependMagickMethod(PackbitsEncodeImage)
#define PaintFloodfillImage  PrependMagickMethod(PaintFloodfillImage)
#define PaintOpaqueImageChannel  PrependMagickMethod(PaintOpaqueImageChannel)
#define PaintOpaqueImage  PrependMagickMethod(PaintOpaqueImage)
#define PaintTransparentImage  PrependMagickMethod(PaintTransparentImage)
#define ParseAbsoluteGeometry  PrependMagickMethod(ParseAbsoluteGeometry)
#define ParseAffineGeometry  PrependMagickMethod(ParseAffineGeometry)
#define ParseChannelOption  PrependMagickMethod(ParseChannelOption)
#define ParseGeometry  PrependMagickMethod(ParseGeometry)
#define ParseGravityGeometry  PrependMagickMethod(ParseGravityGeometry)
#define ParseImageGeometry  PrependMagickMethod(ParseImageGeometry)
#define ParseMagickOption  PrependMagickMethod(ParseMagickOption)
#define ParseMetaGeometry  PrependMagickMethod(ParseMetaGeometry)
#define ParsePageGeometry  PrependMagickMethod(ParsePageGeometry)
#define ParseRegionGeometry  PrependMagickMethod(ParseRegionGeometry)
#define ParseSizeGeometry  PrependMagickMethod(ParseSizeGeometry)
#define PasskeyDecipherImage  PrependMagickMethod(PasskeyDecipherImage)
#define PasskeyEncipherImage  PrependMagickMethod(PasskeyEncipherImage)
#define PersistPixelCache  PrependMagickMethod(PersistPixelCache)
#define PingBlob  PrependMagickMethod(PingBlob)
#define PingImage  PrependMagickMethod(PingImage)
#define PingImages  PrependMagickMethod(PingImages)
#define PlasmaImage  PrependMagickMethod(PlasmaImage)
#define PlasmaImageProxy  PrependMagickMethod(PlasmaImageProxy)
#define PolaroidImage  PrependMagickMethod(PolaroidImage)
#define PopImageList  PrependMagickMethod(PopImageList)
#define PopImagePixels  PrependMagickMethod(PopImagePixels)
#define PosterizeImage  PrependMagickMethod(PosterizeImage)
#define PostscriptGeometry  PrependMagickMethod(PostscriptGeometry)
#define PrependImageToList  PrependMagickMethod(PrependImageToList)
#define PreviewImage  PrependMagickMethod(PreviewImage)
#define PrintStringInfo  PrependMagickMethod(PrintStringInfo)
#define process_message  PrependMagickMethod(process_message)
#define ProfileImage  PrependMagickMethod(ProfileImage)
#define PruneTagFromXMLTree  PrependMagickMethod(PruneTagFromXMLTree)
#define PushImageList  PrependMagickMethod(PushImageList)
#define PushImagePixels  PrependMagickMethod(PushImagePixels)
#define PutEntryInHashmap  PrependMagickMethod(PutEntryInHashmap)
#define QuantizationError  PrependMagickMethod(QuantizationError)
#define QuantizeImage  PrependMagickMethod(QuantizeImage)
#define QuantizeImages  PrependMagickMethod(QuantizeImages)
#define QueryColorDatabase  PrependMagickMethod(QueryColorDatabase)
#define QueryColorname  PrependMagickMethod(QueryColorname)
#define QueryMagickColorname  PrependMagickMethod(QueryMagickColorname)
#define QueryMagickColor  PrependMagickMethod(QueryMagickColor)
#define QueueAuthenticNexus  PrependMagickMethod(QueueAuthenticNexus)
#define QueueAuthenticPixels  PrependMagickMethod(QueueAuthenticPixels)
#define QueueCacheViewAuthenticPixels  PrependMagickMethod(QueueCacheViewAuthenticPixels)
#define RadialBlurImageChannel  PrependMagickMethod(RadialBlurImageChannel)
#define RadialBlurImage  PrependMagickMethod(RadialBlurImage)
#define RaiseImage  PrependMagickMethod(RaiseImage)
#define RandomChannelThresholdImage  PrependMagickMethod(RandomChannelThresholdImage)
#define RandomThresholdImageChannel  PrependMagickMethod(RandomThresholdImageChannel)
#define RandomThresholdImage  PrependMagickMethod(RandomThresholdImage)
#define ReacquireMemory  PrependMagickMethod(ReacquireMemory)
#define ReadBlobByte  PrependMagickMethod(ReadBlobByte)
#define ReadBlobDouble  PrependMagickMethod(ReadBlobDouble)
#define ReadBlobFloat  PrependMagickMethod(ReadBlobFloat)
#define ReadBlobLongLong  PrependMagickMethod(ReadBlobLongLong)
#define ReadBlobLong  PrependMagickMethod(ReadBlobLong)
#define ReadBlobLSBLong  PrependMagickMethod(ReadBlobLSBLong)
#define ReadBlobLSBShort  PrependMagickMethod(ReadBlobLSBShort)
#define ReadBlobMSBLong  PrependMagickMethod(ReadBlobMSBLong)
#define ReadBlobMSBShort  PrependMagickMethod(ReadBlobMSBShort)
#define ReadBlob  PrependMagickMethod(ReadBlob)
#define ReadBlobShort  PrependMagickMethod(ReadBlobShort)
#define ReadBlobString  PrependMagickMethod(ReadBlobString)
#define ReadImage  PrependMagickMethod(ReadImage)
#define ReadImages  PrependMagickMethod(ReadImages)
#define ReadInlineImage  PrependMagickMethod(ReadInlineImage)
#define ReadStream  PrependMagickMethod(ReadStream)
#define RecolorImage  PrependMagickMethod(RecolorImage)
#define ReduceNoiseImage  PrependMagickMethod(ReduceNoiseImage)
#define ReferenceBlob  PrependMagickMethod(ReferenceBlob)
#define ReferenceImage  PrependMagickMethod(ReferenceImage)
#define ReferencePixelCache  PrependMagickMethod(ReferencePixelCache)
#define RegisterARTImage  PrependMagickMethod(RegisterARTImage)
#define RegisterAVIImage  PrependMagickMethod(RegisterAVIImage)
#define RegisterAVSImage  PrependMagickMethod(RegisterAVSImage)
#define RegisterBMPImage  PrependMagickMethod(RegisterBMPImage)
#define RegisterBRAILLEImage  PrependMagickMethod(RegisterBRAILLEImage)
#define RegisterCALSImage  PrependMagickMethod(RegisterCALSImage)
#define RegisterCAPTIONImage  PrependMagickMethod(RegisterCAPTIONImage)
#define RegisterCINImage  PrependMagickMethod(RegisterCINImage)
#define RegisterCIPImage  PrependMagickMethod(RegisterCIPImage)
#define RegisterCLIPImage  PrependMagickMethod(RegisterCLIPImage)
#define RegisterCMYKImage  PrependMagickMethod(RegisterCMYKImage)
#define RegisterCUTImage  PrependMagickMethod(RegisterCUTImage)
#define RegisterDCMImage  PrependMagickMethod(RegisterDCMImage)
#define RegisterDDSImage  PrependMagickMethod(RegisterDDSImage)
#define RegisterDIBImage  PrependMagickMethod(RegisterDIBImage)
#define RegisterDJVUImage  PrependMagickMethod(RegisterDJVUImage)
#define RegisterDNGImage  PrependMagickMethod(RegisterDNGImage)
#define RegisterDOTImage  PrependMagickMethod(RegisterDOTImage)
#define RegisterDPSImage  PrependMagickMethod(RegisterDPSImage)
#define RegisterDPXImage  PrependMagickMethod(RegisterDPXImage)
#define RegisterEPTImage  PrependMagickMethod(RegisterEPTImage)
#define RegisterFAXImage  PrependMagickMethod(RegisterFAXImage)
#define RegisterFITSImage  PrependMagickMethod(RegisterFITSImage)
#define RegisterGIFImage  PrependMagickMethod(RegisterGIFImage)
#define RegisterGRADIENTImage  PrependMagickMethod(RegisterGRADIENTImage)
#define RegisterGRAYImage  PrependMagickMethod(RegisterGRAYImage)
#define RegisterHALDImage  PrependMagickMethod(RegisterHALDImage)
#define RegisterHISTOGRAMImage  PrependMagickMethod(RegisterHISTOGRAMImage)
#define RegisterHRZImage  PrependMagickMethod(RegisterHRZImage)
#define RegisterHTMLImage  PrependMagickMethod(RegisterHTMLImage)
#define RegisterICONImage  PrependMagickMethod(RegisterICONImage)
#define RegisterINFOImage  PrependMagickMethod(RegisterINFOImage)
#define RegisterINLINEImage  PrependMagickMethod(RegisterINLINEImage)
#define RegisterIPLImage  PrependMagickMethod(RegisterIPLImage)
#define RegisterJP2Image  PrependMagickMethod(RegisterJP2Image)
#define RegisterJPEGImage  PrependMagickMethod(RegisterJPEGImage)
#define RegisterLABELImage  PrependMagickMethod(RegisterLABELImage)
#define RegisterMAGICKImage  PrependMagickMethod(RegisterMAGICKImage)
#define RegisterMagickInfo  PrependMagickMethod(RegisterMagickInfo)
#define RegisterMAPImage  PrependMagickMethod(RegisterMAPImage)
#define RegisterMATImage  PrependMagickMethod(RegisterMATImage)
#define RegisterMATTEImage  PrependMagickMethod(RegisterMATTEImage)
#define RegisterMETAImage  PrependMagickMethod(RegisterMETAImage)
#define RegisterMIFFImage  PrependMagickMethod(RegisterMIFFImage)
#define RegisterMONOImage  PrependMagickMethod(RegisterMONOImage)
#define RegisterMPCImage  PrependMagickMethod(RegisterMPCImage)
#define RegisterMPEGImage  PrependMagickMethod(RegisterMPEGImage)
#define RegisterMPRImage  PrependMagickMethod(RegisterMPRImage)
#define RegisterMSLImage  PrependMagickMethod(RegisterMSLImage)
#define RegisterMTVImage  PrependMagickMethod(RegisterMTVImage)
#define RegisterMVGImage  PrependMagickMethod(RegisterMVGImage)
#define RegisterNULLImage  PrependMagickMethod(RegisterNULLImage)
#define RegisterOTBImage  PrependMagickMethod(RegisterOTBImage)
#define RegisterPALMImage  PrependMagickMethod(RegisterPALMImage)
#define RegisterPATTERNImage  PrependMagickMethod(RegisterPATTERNImage)
#define RegisterPCDImage  PrependMagickMethod(RegisterPCDImage)
#define RegisterPCLImage  PrependMagickMethod(RegisterPCLImage)
#define RegisterPCXImage  PrependMagickMethod(RegisterPCXImage)
#define RegisterPDBImage  PrependMagickMethod(RegisterPDBImage)
#define RegisterPDFImage  PrependMagickMethod(RegisterPDFImage)
#define RegisterPICTImage  PrependMagickMethod(RegisterPICTImage)
#define RegisterPIXImage  PrependMagickMethod(RegisterPIXImage)
#define RegisterPLASMAImage  PrependMagickMethod(RegisterPLASMAImage)
#define RegisterPNGImage  PrependMagickMethod(RegisterPNGImage)
#define RegisterPNMImage  PrependMagickMethod(RegisterPNMImage)
#define RegisterPREVIEWImage  PrependMagickMethod(RegisterPREVIEWImage)
#define RegisterPS2Image  PrependMagickMethod(RegisterPS2Image)
#define RegisterPS3Image  PrependMagickMethod(RegisterPS3Image)
#define RegisterPSDImage  PrependMagickMethod(RegisterPSDImage)
#define RegisterPSImage  PrependMagickMethod(RegisterPSImage)
#define RegisterPWPImage  PrependMagickMethod(RegisterPWPImage)
#define RegisterRAWImage  PrependMagickMethod(RegisterRAWImage)
#define RegisterRGBImage  PrependMagickMethod(RegisterRGBImage)
#define RegisterRLAImage  PrependMagickMethod(RegisterRLAImage)
#define RegisterRLEImage  PrependMagickMethod(RegisterRLEImage)
#define RegisterSCRImage  PrependMagickMethod(RegisterSCRImage)
#define RegisterSCTImage  PrependMagickMethod(RegisterSCTImage)
#define RegisterSFWImage  PrependMagickMethod(RegisterSFWImage)
#define RegisterSGIImage  PrependMagickMethod(RegisterSGIImage)
#define RegisterStaticModules  PrependMagickMethod(RegisterStaticModules)
#define RegisterSTEGANOImage  PrependMagickMethod(RegisterSTEGANOImage)
#define RegisterSUNImage  PrependMagickMethod(RegisterSUNImage)
#define RegisterSVGImage  PrependMagickMethod(RegisterSVGImage)
#define RegisterTGAImage  PrependMagickMethod(RegisterTGAImage)
#define RegisterTHUMBNAILImage  PrependMagickMethod(RegisterTHUMBNAILImage)
#define RegisterTIFFImage  PrependMagickMethod(RegisterTIFFImage)
#define RegisterTILEImage  PrependMagickMethod(RegisterTILEImage)
#define RegisterTIMImage  PrependMagickMethod(RegisterTIMImage)
#define RegisterTTFImage  PrependMagickMethod(RegisterTTFImage)
#define RegisterTXTImage  PrependMagickMethod(RegisterTXTImage)
#define RegisterUILImage  PrependMagickMethod(RegisterUILImage)
#define RegisterURLImage  PrependMagickMethod(RegisterURLImage)
#define RegisterUYVYImage  PrependMagickMethod(RegisterUYVYImage)
#define RegisterVICARImage  PrependMagickMethod(RegisterVICARImage)
#define RegisterVIDImage  PrependMagickMethod(RegisterVIDImage)
#define RegisterVIFFImage  PrependMagickMethod(RegisterVIFFImage)
#define RegisterWBMPImage  PrependMagickMethod(RegisterWBMPImage)
#define RegisterWPGImage  PrependMagickMethod(RegisterWPGImage)
#define RegisterXBMImage  PrependMagickMethod(RegisterXBMImage)
#define RegisterXCFImage  PrependMagickMethod(RegisterXCFImage)
#define RegisterXCImage  PrependMagickMethod(RegisterXCImage)
#define RegisterXImage  PrependMagickMethod(RegisterXImage)
#define RegisterXPMImage  PrependMagickMethod(RegisterXPMImage)
#define RegisterXPSImage  PrependMagickMethod(RegisterXPSImage)
#define RegisterXWDImage  PrependMagickMethod(RegisterXWDImage)
#define RegisterYCBCRImage  PrependMagickMethod(RegisterYCBCRImage)
#define RegisterYUVImage  PrependMagickMethod(RegisterYUVImage)
#define RelinquishAlignedMemory  PrependMagickMethod(RelinquishAlignedMemory)
#define RelinquishMagickMatrix  PrependMagickMethod(RelinquishMagickMatrix)
#define RelinquishMagickMemory  PrependMagickMethod(RelinquishMagickMemory)
#define RelinquishMagickResource  PrependMagickMethod(RelinquishMagickResource)
#define RelinquishSemaphoreInfo  PrependMagickMethod(RelinquishSemaphoreInfo)
#define RelinquishUniqueFileResource  PrependMagickMethod(RelinquishUniqueFileResource)
#define RemapImage  PrependMagickMethod(RemapImage)
#define RemapImages  PrependMagickMethod(RemapImages)
#define RemoteDisplayCommand  PrependMagickMethod(RemoteDisplayCommand)
#define RemoveDuplicateLayers  PrependMagickMethod(RemoveDuplicateLayers)
#define RemoveElementByValueFromLinkedList  PrependMagickMethod(RemoveElementByValueFromLinkedList)
#define RemoveElementFromLinkedList  PrependMagickMethod(RemoveElementFromLinkedList)
#define RemoveEntryFromHashmap  PrependMagickMethod(RemoveEntryFromHashmap)
#define RemoveFirstImageFromList  PrependMagickMethod(RemoveFirstImageFromList)
#define RemoveImageArtifact  PrependMagickMethod(RemoveImageArtifact)
#define RemoveImageFromList  PrependMagickMethod(RemoveImageFromList)
#define RemoveImageOption  PrependMagickMethod(RemoveImageOption)
#define RemoveImageProfile  PrependMagickMethod(RemoveImageProfile)
#define RemoveImageProperty  PrependMagickMethod(RemoveImageProperty)
#define RemoveImageRegistry  PrependMagickMethod(RemoveImageRegistry)
#define RemoveLastElementFromLinkedList  PrependMagickMethod(RemoveLastElementFromLinkedList)
#define RemoveLastImageFromList  PrependMagickMethod(RemoveLastImageFromList)
#define RemoveNodeByValueFromSplayTree  PrependMagickMethod(RemoveNodeByValueFromSplayTree)
#define RemoveNodeFromSplayTree  PrependMagickMethod(RemoveNodeFromSplayTree)
#define RemoveZeroDelayLayers  PrependMagickMethod(RemoveZeroDelayLayers)
#define ReplaceImageInList  PrependMagickMethod(ReplaceImageInList)
#define ResampleImage  PrependMagickMethod(ResampleImage)
#define ResamplePixelColor  PrependMagickMethod(ResamplePixelColor)
#define ResetHashmapIterator  PrependMagickMethod(ResetHashmapIterator)
#define ResetImageArtifactIterator  PrependMagickMethod(ResetImageArtifactIterator)
#define ResetImageAttributeIterator  PrependMagickMethod(ResetImageAttributeIterator)
#define ResetImageOptionIterator  PrependMagickMethod(ResetImageOptionIterator)
#define ResetImagePage  PrependMagickMethod(ResetImagePage)
#define ResetImageProfileIterator  PrependMagickMethod(ResetImageProfileIterator)
#define ResetImagePropertyIterator  PrependMagickMethod(ResetImagePropertyIterator)
#define ResetImageRegistryIterator  PrependMagickMethod(ResetImageRegistryIterator)
#define ResetLinkedListIterator  PrependMagickMethod(ResetLinkedListIterator)
#define ResetMagickMemory  PrependMagickMethod(ResetMagickMemory)
#define ResetSplayTreeIterator  PrependMagickMethod(ResetSplayTreeIterator)
#define ResetStringInfo  PrependMagickMethod(ResetStringInfo)
#define ResetTimer  PrependMagickMethod(ResetTimer)
#define ResizeImage  PrependMagickMethod(ResizeImage)
#define ResizeMagickMemory  PrependMagickMethod(ResizeMagickMemory)
#define ResizeQuantumMemory  PrependMagickMethod(ResizeQuantumMemory)
#define ReverseImageList  PrependMagickMethod(ReverseImageList)
#define RGBTransformImage  PrependMagickMethod(RGBTransformImage)
#define RollImage  PrependMagickMethod(RollImage)
#define RotateImage  PrependMagickMethod(RotateImage)
#define SampleImage  PrependMagickMethod(SampleImage)
#define ScaleImage  PrependMagickMethod(ScaleImage)
#define ScaleResampleFilter  PrependMagickMethod(ScaleResampleFilter)
#define SeedPseudoRandomGenerator  PrependMagickMethod(SeedPseudoRandomGenerator)
#define SeekBlob  PrependMagickMethod(SeekBlob)
#define SegmentImage  PrependMagickMethod(SegmentImage)
#define SelectiveBlurImageChannel  PrependMagickMethod(SelectiveBlurImageChannel)
#define SelectiveBlurImage  PrependMagickMethod(SelectiveBlurImage)
#define SeparateImageChannel  PrependMagickMethod(SeparateImageChannel)
#define SeparateImages  PrependMagickMethod(SeparateImages)
#define SepiaToneImage  PrependMagickMethod(SepiaToneImage)
#define SetBlobExempt  PrependMagickMethod(SetBlobExempt)
#define SetBlobExtent  PrependMagickMethod(SetBlobExtent)
#define SetCacheThreshold  PrependMagickMethod(SetCacheThreshold)
#define SetCacheViewPixels  PrependMagickMethod(SetCacheViewPixels)
#define SetCacheViewStorageClass  PrependMagickMethod(SetCacheViewStorageClass)
#define SetCacheViewVirtualPixelMethod  PrependMagickMethod(SetCacheViewVirtualPixelMethod)
#define SetClientName  PrependMagickMethod(SetClientName)
#define SetClientPath  PrependMagickMethod(SetClientPath)
#define SetErrorHandler  PrependMagickMethod(SetErrorHandler)
#define SetExceptionInfo  PrependMagickMethod(SetExceptionInfo)
#define SetFatalErrorHandler  PrependMagickMethod(SetFatalErrorHandler)
#define SetGeometryInfo  PrependMagickMethod(SetGeometryInfo)
#define SetGeometry  PrependMagickMethod(SetGeometry)
#define SetHeaderFromIPL  PrependMagickMethod(SetHeaderFromIPL)
#define SetImageAlphaChannel  PrependMagickMethod(SetImageAlphaChannel)
#define SetImageArtifact  PrependMagickMethod(SetImageArtifact)
#define SetImageAttribute  PrependMagickMethod(SetImageAttribute)
#define SetImageBackgroundColor  PrependMagickMethod(SetImageBackgroundColor)
#define SetImageChannelDepth  PrependMagickMethod(SetImageChannelDepth)
#define SetImageClipMask  PrependMagickMethod(SetImageClipMask)
#define SetImageColorspace  PrependMagickMethod(SetImageColorspace)
#define SetImageDepth  PrependMagickMethod(SetImageDepth)
#define SetImageExtent  PrependMagickMethod(SetImageExtent)
#define SetImageInfoBlob  PrependMagickMethod(SetImageInfoBlob)
#define SetImageInfoFile  PrependMagickMethod(SetImageInfoFile)
#define SetImageInfo  PrependMagickMethod(SetImageInfo)
#define SetImageInfoProgressMonitor  PrependMagickMethod(SetImageInfoProgressMonitor)
#define SetImageList  PrependMagickMethod(SetImageList)
#define SetImageMask  PrependMagickMethod(SetImageMask)
#define SetImageOpacity  PrependMagickMethod(SetImageOpacity)
#define SetImageOption  PrependMagickMethod(SetImageOption)
#define SetImagePixels  PrependMagickMethod(SetImagePixels)
#define SetImage  PrependMagickMethod(SetImage)
#define SetImageProfile  PrependMagickMethod(SetImageProfile)
#define SetImageProgressMonitor  PrependMagickMethod(SetImageProgressMonitor)
#define SetImageProperty  PrependMagickMethod(SetImageProperty)
#define SetImageRegistry  PrependMagickMethod(SetImageRegistry)
#define SetImageStorageClass  PrependMagickMethod(SetImageStorageClass)
#define SetImageType  PrependMagickMethod(SetImageType)
#define SetImageVirtualPixelMethod  PrependMagickMethod(SetImageVirtualPixelMethod)
#define SetLogEventMask  PrependMagickMethod(SetLogEventMask)
#define SetLogFormat  PrependMagickMethod(SetLogFormat)
#define SetLogName  PrependMagickMethod(SetLogName)
#define SetMagickInfo  PrependMagickMethod(SetMagickInfo)
#define SetMagickMemoryMethods  PrependMagickMethod(SetMagickMemoryMethods)
#define SetMagickRegistry  PrependMagickMethod(SetMagickRegistry)
#define SetMagickResourceLimit  PrependMagickMethod(SetMagickResourceLimit)
#define SetMonitorHandler  PrependMagickMethod(SetMonitorHandler)
#define SetPixelCacheMethods  PrependMagickMethod(SetPixelCacheMethods)
#define SetPixelCacheVirtualMethod  PrependMagickMethod(SetPixelCacheVirtualMethod)
#define SetQuantumAlphaType  PrependMagickMethod(SetQuantumAlphaType)
#define SetQuantumDepth  PrependMagickMethod(SetQuantumDepth)
#define SetQuantumFormat  PrependMagickMethod(SetQuantumFormat)
#define SetQuantumImageType  PrependMagickMethod(SetQuantumImageType)
#define SetQuantumMinIsWhite  PrependMagickMethod(SetQuantumMinIsWhite)
#define SetQuantumPack  PrependMagickMethod(SetQuantumPack)
#define SetQuantumPad  PrependMagickMethod(SetQuantumPad)
#define SetQuantumQuantum  PrependMagickMethod(SetQuantumQuantum)
#define SetQuantumScale  PrependMagickMethod(SetQuantumScale)
#define SetRandomKey  PrependMagickMethod(SetRandomKey)
#define SetRandomTrueRandom  PrependMagickMethod(SetRandomTrueRandom)
#define SetResampleFilterInterpolateMethod  PrependMagickMethod(SetResampleFilterInterpolateMethod)
#define SetResampleFilter  PrependMagickMethod(SetResampleFilter)
#define SetResampleFilterVirtualPixelMethod  PrependMagickMethod(SetResampleFilterVirtualPixelMethod)
#define SetResizeFilterSupport  PrependMagickMethod(SetResizeFilterSupport)
#define SetSignatureDigest  PrependMagickMethod(SetSignatureDigest)
#define SetStreamInfoClientData  PrependMagickMethod(SetStreamInfoClientData)
#define SetStreamInfoMap  PrependMagickMethod(SetStreamInfoMap)
#define SetStreamInfoStorageType  PrependMagickMethod(SetStreamInfoStorageType)
#define SetStringInfoDatum  PrependMagickMethod(SetStringInfoDatum)
#define SetStringInfoLength  PrependMagickMethod(SetStringInfoLength)
#define SetStringInfoPath  PrependMagickMethod(SetStringInfoPath)
#define SetStringInfo  PrependMagickMethod(SetStringInfo)
#define SetWarningHandler  PrependMagickMethod(SetWarningHandler)
#define SetXMLTreeAttribute  PrependMagickMethod(SetXMLTreeAttribute)
#define SetXMLTreeContent  PrependMagickMethod(SetXMLTreeContent)
#define ShadeImage  PrependMagickMethod(ShadeImage)
#define ShadowImage  PrependMagickMethod(ShadowImage)
#define SharpenImageChannel  PrependMagickMethod(SharpenImageChannel)
#define SharpenImage  PrependMagickMethod(SharpenImage)
#define ShaveImage  PrependMagickMethod(ShaveImage)
#define ShearImage  PrependMagickMethod(ShearImage)
#define ShiftImageList  PrependMagickMethod(ShiftImageList)
#define SigmoidalContrastImageChannel  PrependMagickMethod(SigmoidalContrastImageChannel)
#define SigmoidalContrastImage  PrependMagickMethod(SigmoidalContrastImage)
#define SignatureImage  PrependMagickMethod(SignatureImage)
#define SimilarityImage  PrependMagickMethod(SimilarityImage)
#define SizeBlob  PrependMagickMethod(SizeBlob)
#define SketchImage  PrependMagickMethod(SketchImage)
#define SolarizeImage  PrependMagickMethod(SolarizeImage)
#define SortColormapByIntensity  PrependMagickMethod(SortColormapByIntensity)
#define SparseColorImage  PrependMagickMethod(SparseColorImage)
#define SpliceImageIntoList  PrependMagickMethod(SpliceImageIntoList)
#define SpliceImageList  PrependMagickMethod(SpliceImageList)
#define SpliceImage  PrependMagickMethod(SpliceImage)
#define SplitImageList  PrependMagickMethod(SplitImageList)
#define SplitStringInfo  PrependMagickMethod(SplitStringInfo)
#define SpreadImage  PrependMagickMethod(SpreadImage)
#define StartTimer  PrependMagickMethod(StartTimer)
#define SteganoImage  PrependMagickMethod(SteganoImage)
#define StereoAnaglyphImage  PrependMagickMethod(StereoAnaglyphImage)
#define StereoImage  PrependMagickMethod(StereoImage)
#define StreamImage  PrependMagickMethod(StreamImage)
#define StringInfoToHexString  PrependMagickMethod(StringInfoToHexString)
#define StringInfoToString  PrependMagickMethod(StringInfoToString)
#define StringToArgv  PrependMagickMethod(StringToArgv)
#define StringToDouble  PrependMagickMethod(StringToDouble)
#define StringToken  PrependMagickMethod(StringToken)
#define StringToList  PrependMagickMethod(StringToList)
#define StringToStringInfo  PrependMagickMethod(StringToStringInfo)
#define StripImage  PrependMagickMethod(StripImage)
#define Strip  PrependMagickMethod(Strip)
#define StripString  PrependMagickMethod(StripString)
#define SubstituteString  PrependMagickMethod(SubstituteString)
#define SwirlImage  PrependMagickMethod(SwirlImage)
#define SyncAuthenticPixelCacheNexus  PrependMagickMethod(SyncAuthenticPixelCacheNexus)
#define SyncAuthenticPixels  PrependMagickMethod(SyncAuthenticPixels)
#define SyncCacheViewAuthenticPixels  PrependMagickMethod(SyncCacheViewAuthenticPixels)
#define SyncCacheViewPixels  PrependMagickMethod(SyncCacheViewPixels)
#define SyncCacheView  PrependMagickMethod(SyncCacheView)
#define SyncImageList  PrependMagickMethod(SyncImageList)
#define SyncImagePixels  PrependMagickMethod(SyncImagePixels)
#define SyncImage  PrependMagickMethod(SyncImage)
#define SyncImageProfiles  PrependMagickMethod(SyncImageProfiles)
#define SyncImageSettings  PrependMagickMethod(SyncImageSettings)
#define SyncImagesSettings  PrependMagickMethod(SyncImagesSettings)
#define SyncNextImageInList  PrependMagickMethod(SyncNextImageInList)
#define SystemCommand  PrependMagickMethod(SystemCommand)
#define TellBlob  PrependMagickMethod(TellBlob)
#define TemporaryFilename  PrependMagickMethod(TemporaryFilename)
#define TextureImage  PrependMagickMethod(TextureImage)
#define ThresholdImageChannel  PrependMagickMethod(ThresholdImageChannel)
#define ThresholdImage  PrependMagickMethod(ThresholdImage)
#define ThrowException  PrependMagickMethod(ThrowException)
#define ThrowMagickExceptionList  PrependMagickMethod(ThrowMagickExceptionList)
#define ThrowMagickException  PrependMagickMethod(ThrowMagickException)
#define ThumbnailImage  PrependMagickMethod(ThumbnailImage)
#define TintImage  PrependMagickMethod(TintImage)
#define Tokenizer  PrependMagickMethod(Tokenizer)
#define TransformColorspace  PrependMagickMethod(TransformColorspace)
#define TransformHSL  PrependMagickMethod(TransformHSL)
#define TransformImageColorspace  PrependMagickMethod(TransformImageColorspace)
#define TransformImage  PrependMagickMethod(TransformImage)
#define TransformImages  PrependMagickMethod(TransformImages)
#define TransformRGBImage  PrependMagickMethod(TransformRGBImage)
#define TranslateText  PrependMagickMethod(TranslateText)
#define TransparentImage  PrependMagickMethod(TransparentImage)
#define TransparentPaintImageChroma  PrependMagickMethod(TransparentPaintImageChroma)
#define TransparentPaintImage  PrependMagickMethod(TransparentPaintImage)
#define TransposeImage  PrependMagickMethod(TransposeImage)
#define TransverseImage  PrependMagickMethod(TransverseImage)
#define TrimImage  PrependMagickMethod(TrimImage)
#define UniqueImageColors  PrependMagickMethod(UniqueImageColors)
#define UnlockSemaphoreInfo  PrependMagickMethod(UnlockSemaphoreInfo)
#define UnmapBlob  PrependMagickMethod(UnmapBlob)
#define UnregisterARTImage  PrependMagickMethod(UnregisterARTImage)
#define UnregisterAVIImage  PrependMagickMethod(UnregisterAVIImage)
#define UnregisterAVSImage  PrependMagickMethod(UnregisterAVSImage)
#define UnregisterBMPImage  PrependMagickMethod(UnregisterBMPImage)
#define UnregisterBRAILLEImage  PrependMagickMethod(UnregisterBRAILLEImage)
#define UnregisterCALSImage  PrependMagickMethod(UnregisterCALSImage)
#define UnregisterCAPTIONImage  PrependMagickMethod(UnregisterCAPTIONImage)
#define UnregisterCINImage  PrependMagickMethod(UnregisterCINImage)
#define UnregisterCIPImage  PrependMagickMethod(UnregisterCIPImage)
#define UnregisterCLIPImage  PrependMagickMethod(UnregisterCLIPImage)
#define UnregisterCMYKImage  PrependMagickMethod(UnregisterCMYKImage)
#define UnregisterCUTImage  PrependMagickMethod(UnregisterCUTImage)
#define UnregisterDCMImage  PrependMagickMethod(UnregisterDCMImage)
#define UnregisterDDSImage  PrependMagickMethod(UnregisterDDSImage)
#define UnregisterDIBImage  PrependMagickMethod(UnregisterDIBImage)
#define UnregisterDJVUImage  PrependMagickMethod(UnregisterDJVUImage)
#define UnregisterDNGImage  PrependMagickMethod(UnregisterDNGImage)
#define UnregisterDOTImage  PrependMagickMethod(UnregisterDOTImage)
#define UnregisterDPSImage  PrependMagickMethod(UnregisterDPSImage)
#define UnregisterDPXImage  PrependMagickMethod(UnregisterDPXImage)
#define UnregisterEPTImage  PrependMagickMethod(UnregisterEPTImage)
#define UnregisterFAXImage  PrependMagickMethod(UnregisterFAXImage)
#define UnregisterFITSImage  PrependMagickMethod(UnregisterFITSImage)
#define UnregisterGIFImage  PrependMagickMethod(UnregisterGIFImage)
#define UnregisterGRADIENTImage  PrependMagickMethod(UnregisterGRADIENTImage)
#define UnregisterGRAYImage  PrependMagickMethod(UnregisterGRAYImage)
#define UnregisterHALDImage  PrependMagickMethod(UnregisterHALDImage)
#define UnregisterHISTOGRAMImage  PrependMagickMethod(UnregisterHISTOGRAMImage)
#define UnregisterHRZImage  PrependMagickMethod(UnregisterHRZImage)
#define UnregisterHTMLImage  PrependMagickMethod(UnregisterHTMLImage)
#define UnregisterICONImage  PrependMagickMethod(UnregisterICONImage)
#define UnregisterINFOImage  PrependMagickMethod(UnregisterINFOImage)
#define UnregisterINLINEImage  PrependMagickMethod(UnregisterINLINEImage)
#define UnregisterIPLImage  PrependMagickMethod(UnregisterIPLImage)
#define UnregisterJP2Image  PrependMagickMethod(UnregisterJP2Image)
#define UnregisterJPEGImage  PrependMagickMethod(UnregisterJPEGImage)
#define UnregisterLABELImage  PrependMagickMethod(UnregisterLABELImage)
#define UnregisterMAGICKImage  PrependMagickMethod(UnregisterMAGICKImage)
#define UnregisterMagickInfo  PrependMagickMethod(UnregisterMagickInfo)
#define UnregisterMAPImage  PrependMagickMethod(UnregisterMAPImage)
#define UnregisterMATImage  PrependMagickMethod(UnregisterMATImage)
#define UnregisterMATTEImage  PrependMagickMethod(UnregisterMATTEImage)
#define UnregisterMETAImage  PrependMagickMethod(UnregisterMETAImage)
#define UnregisterMIFFImage  PrependMagickMethod(UnregisterMIFFImage)
#define UnregisterMONOImage  PrependMagickMethod(UnregisterMONOImage)
#define UnregisterMPCImage  PrependMagickMethod(UnregisterMPCImage)
#define UnregisterMPEGImage  PrependMagickMethod(UnregisterMPEGImage)
#define UnregisterMPRImage  PrependMagickMethod(UnregisterMPRImage)
#define UnregisterMSLImage  PrependMagickMethod(UnregisterMSLImage)
#define UnregisterMTVImage  PrependMagickMethod(UnregisterMTVImage)
#define UnregisterMVGImage  PrependMagickMethod(UnregisterMVGImage)
#define UnregisterNULLImage  PrependMagickMethod(UnregisterNULLImage)
#define UnregisterOTBImage  PrependMagickMethod(UnregisterOTBImage)
#define UnregisterPALMImage  PrependMagickMethod(UnregisterPALMImage)
#define UnregisterPATTERNImage  PrependMagickMethod(UnregisterPATTERNImage)
#define UnregisterPCDImage  PrependMagickMethod(UnregisterPCDImage)
#define UnregisterPCLImage  PrependMagickMethod(UnregisterPCLImage)
#define UnregisterPCXImage  PrependMagickMethod(UnregisterPCXImage)
#define UnregisterPDBImage  PrependMagickMethod(UnregisterPDBImage)
#define UnregisterPDFImage  PrependMagickMethod(UnregisterPDFImage)
#define UnregisterPICTImage  PrependMagickMethod(UnregisterPICTImage)
#define UnregisterPIXImage  PrependMagickMethod(UnregisterPIXImage)
#define UnregisterPLASMAImage  PrependMagickMethod(UnregisterPLASMAImage)
#define UnregisterPNGImage  PrependMagickMethod(UnregisterPNGImage)
#define UnregisterPNMImage  PrependMagickMethod(UnregisterPNMImage)
#define UnregisterPREVIEWImage  PrependMagickMethod(UnregisterPREVIEWImage)
#define UnregisterPS2Image  PrependMagickMethod(UnregisterPS2Image)
#define UnregisterPS3Image  PrependMagickMethod(UnregisterPS3Image)
#define UnregisterPSDImage  PrependMagickMethod(UnregisterPSDImage)
#define UnregisterPSImage  PrependMagickMethod(UnregisterPSImage)
#define UnregisterPWPImage  PrependMagickMethod(UnregisterPWPImage)
#define UnregisterRAWImage  PrependMagickMethod(UnregisterRAWImage)
#define UnregisterRGBImage  PrependMagickMethod(UnregisterRGBImage)
#define UnregisterRLAImage  PrependMagickMethod(UnregisterRLAImage)
#define UnregisterRLEImage  PrependMagickMethod(UnregisterRLEImage)
#define UnregisterSCRImage  PrependMagickMethod(UnregisterSCRImage)
#define UnregisterSCTImage  PrependMagickMethod(UnregisterSCTImage)
#define UnregisterSFWImage  PrependMagickMethod(UnregisterSFWImage)
#define UnregisterSGIImage  PrependMagickMethod(UnregisterSGIImage)
#define UnregisterStaticModules  PrependMagickMethod(UnregisterStaticModules)
#define UnregisterSTEGANOImage  PrependMagickMethod(UnregisterSTEGANOImage)
#define UnregisterSUNImage  PrependMagickMethod(UnregisterSUNImage)
#define UnregisterSVGImage  PrependMagickMethod(UnregisterSVGImage)
#define UnregisterTGAImage  PrependMagickMethod(UnregisterTGAImage)
#define UnregisterTHUMBNAILImage  PrependMagickMethod(UnregisterTHUMBNAILImage)
#define UnregisterTIFFImage  PrependMagickMethod(UnregisterTIFFImage)
#define UnregisterTILEImage  PrependMagickMethod(UnregisterTILEImage)
#define UnregisterTIMImage  PrependMagickMethod(UnregisterTIMImage)
#define UnregisterTTFImage  PrependMagickMethod(UnregisterTTFImage)
#define UnregisterTXTImage  PrependMagickMethod(UnregisterTXTImage)
#define UnregisterUILImage  PrependMagickMethod(UnregisterUILImage)
#define UnregisterURLImage  PrependMagickMethod(UnregisterURLImage)
#define UnregisterUYVYImage  PrependMagickMethod(UnregisterUYVYImage)
#define UnregisterVICARImage  PrependMagickMethod(UnregisterVICARImage)
#define UnregisterVIDImage  PrependMagickMethod(UnregisterVIDImage)
#define UnregisterVIFFImage  PrependMagickMethod(UnregisterVIFFImage)
#define UnregisterWBMPImage  PrependMagickMethod(UnregisterWBMPImage)
#define UnregisterWPGImage  PrependMagickMethod(UnregisterWPGImage)
#define UnregisterXBMImage  PrependMagickMethod(UnregisterXBMImage)
#define UnregisterXCFImage  PrependMagickMethod(UnregisterXCFImage)
#define UnregisterXCImage  PrependMagickMethod(UnregisterXCImage)
#define UnregisterXImage  PrependMagickMethod(UnregisterXImage)
#define UnregisterXPMImage  PrependMagickMethod(UnregisterXPMImage)
#define UnregisterXPSImage  PrependMagickMethod(UnregisterXPSImage)
#define UnregisterXWDImage  PrependMagickMethod(UnregisterXWDImage)
#define UnregisterYCBCRImage  PrependMagickMethod(UnregisterYCBCRImage)
#define UnregisterYUVImage  PrependMagickMethod(UnregisterYUVImage)
#define UnsharpMaskImageChannel  PrependMagickMethod(UnsharpMaskImageChannel)
#define UnsharpMaskImage  PrependMagickMethod(UnsharpMaskImage)
#define UnshiftImageList  PrependMagickMethod(UnshiftImageList)
#define UpdateSignature  PrependMagickMethod(UpdateSignature)
#define ValidateColormapIndex  PrependMagickMethod(ValidateColormapIndex)
#define VignetteImage  PrependMagickMethod(VignetteImage)
#define WaveImage  PrependMagickMethod(WaveImage)
#define WhiteThresholdImageChannel  PrependMagickMethod(WhiteThresholdImageChannel)
#define WhiteThresholdImage  PrependMagickMethod(WhiteThresholdImage)
#define WriteBlobByte  PrependMagickMethod(WriteBlobByte)
#define WriteBlobFloat  PrependMagickMethod(WriteBlobFloat)
#define WriteBlobLong  PrependMagickMethod(WriteBlobLong)
#define WriteBlobLSBLong  PrependMagickMethod(WriteBlobLSBLong)
#define WriteBlobLSBShort  PrependMagickMethod(WriteBlobLSBShort)
#define WriteBlobMSBLong  PrependMagickMethod(WriteBlobMSBLong)
#define WriteBlobMSBShort  PrependMagickMethod(WriteBlobMSBShort)
#define WriteBlob  PrependMagickMethod(WriteBlob)
#define WriteBlobShort  PrependMagickMethod(WriteBlobShort)
#define WriteBlobString  PrependMagickMethod(WriteBlobString)
#define WriteImage  PrependMagickMethod(WriteImage)
#define WriteImages  PrependMagickMethod(WriteImages)
#define WriteStream  PrependMagickMethod(WriteStream)
#define XAnimateBackgroundImage  PrependMagickMethod(XAnimateBackgroundImage)
#define XAnimateImages  PrependMagickMethod(XAnimateImages)
#define XAnnotateImage  PrependMagickMethod(XAnnotateImage)
#define XBestFont  PrependMagickMethod(XBestFont)
#define XBestIconSize  PrependMagickMethod(XBestIconSize)
#define XBestPixel  PrependMagickMethod(XBestPixel)
#define XBestVisualInfo  PrependMagickMethod(XBestVisualInfo)
#define XCheckDefineCursor  PrependMagickMethod(XCheckDefineCursor)
#define XCheckRefreshWindows  PrependMagickMethod(XCheckRefreshWindows)
#define XClientMessage  PrependMagickMethod(XClientMessage)
#define XColorBrowserWidget  PrependMagickMethod(XColorBrowserWidget)
#define XCommandWidget  PrependMagickMethod(XCommandWidget)
#define XConfigureImageColormap  PrependMagickMethod(XConfigureImageColormap)
#define XConfirmWidget  PrependMagickMethod(XConfirmWidget)
#define XConstrainWindowPosition  PrependMagickMethod(XConstrainWindowPosition)
#define XDelay  PrependMagickMethod(XDelay)
#define XDestroyResourceInfo  PrependMagickMethod(XDestroyResourceInfo)
#define XDestroyWindowColors  PrependMagickMethod(XDestroyWindowColors)
#define XDialogWidget  PrependMagickMethod(XDialogWidget)
#define XDisplayBackgroundImage  PrependMagickMethod(XDisplayBackgroundImage)
#define XDisplayImageInfo  PrependMagickMethod(XDisplayImageInfo)
#define XDisplayImage  PrependMagickMethod(XDisplayImage)
#define XDrawImage  PrependMagickMethod(XDrawImage)
#define XError  PrependMagickMethod(XError)
#define XFileBrowserWidget  PrependMagickMethod(XFileBrowserWidget)
#define XFontBrowserWidget  PrependMagickMethod(XFontBrowserWidget)
#define XFreeResources  PrependMagickMethod(XFreeResources)
#define XFreeStandardColormap  PrependMagickMethod(XFreeStandardColormap)
#define XGetAnnotateInfo  PrependMagickMethod(XGetAnnotateInfo)
#define XGetImportInfo  PrependMagickMethod(XGetImportInfo)
#define XGetMapInfo  PrependMagickMethod(XGetMapInfo)
#define XGetPixelPacket  PrependMagickMethod(XGetPixelPacket)
#define XGetResourceClass  PrependMagickMethod(XGetResourceClass)
#define XGetResourceDatabase  PrependMagickMethod(XGetResourceDatabase)
#define XGetResourceInfo  PrependMagickMethod(XGetResourceInfo)
#define XGetResourceInstance  PrependMagickMethod(XGetResourceInstance)
#define XGetScreenDensity  PrependMagickMethod(XGetScreenDensity)
#define XGetWindowColor  PrependMagickMethod(XGetWindowColor)
#define XGetWindowInfo  PrependMagickMethod(XGetWindowInfo)
#define XHighlightEllipse  PrependMagickMethod(XHighlightEllipse)
#define XHighlightLine  PrependMagickMethod(XHighlightLine)
#define XHighlightRectangle  PrependMagickMethod(XHighlightRectangle)
#define XImportImage  PrependMagickMethod(XImportImage)
#define XInfoWidget  PrependMagickMethod(XInfoWidget)
#define XInitializeWindows  PrependMagickMethod(XInitializeWindows)
#define XListBrowserWidget  PrependMagickMethod(XListBrowserWidget)
#define XMagickProgressMonitor  PrependMagickMethod(XMagickProgressMonitor)
#define XMakeCursor  PrependMagickMethod(XMakeCursor)
#define XMakeImage  PrependMagickMethod(XMakeImage)
#define XMakeMagnifyImage  PrependMagickMethod(XMakeMagnifyImage)
#define XMakeStandardColormap  PrependMagickMethod(XMakeStandardColormap)
#define XMakeWindow  PrependMagickMethod(XMakeWindow)
#define XMenuWidget  PrependMagickMethod(XMenuWidget)
#define XMLTreeInfoToXML  PrependMagickMethod(XMLTreeInfoToXML)
#define XNoticeWidget  PrependMagickMethod(XNoticeWidget)
#define XPreferencesWidget  PrependMagickMethod(XPreferencesWidget)
#define XProgressMonitorWidget  PrependMagickMethod(XProgressMonitorWidget)
#define XQueryColorDatabase  PrependMagickMethod(XQueryColorDatabase)
#define XQueryPosition  PrependMagickMethod(XQueryPosition)
#define XRefreshWindow  PrependMagickMethod(XRefreshWindow)
#define XRemoteCommand  PrependMagickMethod(XRemoteCommand)
#define XRetainWindowColors  PrependMagickMethod(XRetainWindowColors)
#define XSetCursorState  PrependMagickMethod(XSetCursorState)
#define XSetWindows  PrependMagickMethod(XSetWindows)
#define XTextViewWidget  PrependMagickMethod(XTextViewWidget)
#define XUserPreferences  PrependMagickMethod(XUserPreferences)
#define XWarning  PrependMagickMethod(XWarning)
#define XWindowByID  PrependMagickMethod(XWindowByID)
#define XWindowByName  PrependMagickMethod(XWindowByName)
#define XWindowByProperty  PrependMagickMethod(XWindowByProperty)
#define ZLIBEncodeImage  PrependMagickMethod(ZLIBEncodeImage)
#define ZoomImage  PrependMagickMethod(ZoomImage)
#endif

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
