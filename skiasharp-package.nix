# Adapted from
# https://github.com/NixOS/nixpkgs/blob/360e50e1fb66f045f119c88ef4c45387c9e615b3/pkgs/by-name/sk/skia/package.nix.
{
  lib,
  stdenv,
  fetchgit,
  expat,
  fontconfig,
  freetype,
  harfbuzzFull,
  icu,
  gn,
  libGL,
  libjpeg,
  libwebp,
  libX11,
  ninja,
  python3,
  testers,
  vulkan-headers,
  vulkan-memory-allocator,
  xcbuild,
  cctools,
  zlib,
  fixDarwinDylibNames,

  enableVulkan ? !stdenv.hostPlatform.isDarwin,
  enableWuffs ? true,
}:

stdenv.mkDerivation (finalAttrs:
  let
    # See https://github.com/mono/SkiaSharp/wiki/Building-on-Linux for how mono
    # skia is built.
    skiasharp-src = fetchgit {
      url = "https://github.com/mono/skia.git";
      rev = "30abf4a0dcd3006f3a555e46906151e486f87b6e";
      hash = "sha256-Jmw1+lMUn81aNkHuM0l0n1HBAMy6avaA1UM5Z2ftmsk=";
    };
  
    # For skia's GifDecoder
    wuffs-src = fetchgit {
      url = "https://github.com/google/wuffs.git";
      rev = "7250990196ab0306f16b639feefbbca4d5323080";
      hash = "sha256-84Enc72zz3hWlzbcK1gxg79GzPD6NXfIq4Utmqj2ohc=";
    };

    # NOTE: workaround for
    # > ninja: error: '../../third_party/externals/harfbuzz/src/harfbuzz-subset.cc', needed by 'obj/third_party/externals/harfbuzz/src/libHarfBuzzSharp.harfbuzz-subset.o', missing and no known rule to make it
    #
    # The original package.nix on Nixpkgs for skia uses the system's harfbuzz.
    harfbuzz-src = fetchgit {
      url = "https://github.com/harfbuzz/harfbuzz.git";
      rev = "5933468261edffd58c2343a3a8705d1563f07cff";
      hash = "sha256-AoQ2HSXCPmWUPbyOsCjqXdjCjiOc+G6mGq2LwLrSxf8=";
    };
  in
  {
    pname = "skiasharp";
    version = "skiasharp-30abf4a0dcd3006f3a555e46906151e486f87b6e";

    srcs = [ skiasharp-src wuffs-src harfbuzz-src ];

    sourceRoot = "${skiasharp-src.name}";

    # We want SkGifDecoder, which requires wuffs, which requires manually
    # copy/pasting wuff stb files to /third_party/externals/. See skia's wuff's
    # BUILD.gn files. Do 'rg SkGifDecoder/skia_use_wuffs' on Skia's source to
    # dig up more info.
    #
    # Also SkiaSharp wants 'harfbuzz' to be pulled. TODO: Use 'harfbuzzFull'?
    preConfigure = ''
      mkdir ./third_party/externals
      ln -s ${wuffs-src} ./third_party/externals/wuffs
      ln -s ${harfbuzz-src} ./third_party/externals/harfbuzz
    '';

    postPatch = ''
      # System zlib detection bug workaround
      substituteInPlace BUILD.gn \
        --replace-fail 'deps = [ "//third_party/zlib" ]' 'deps = []'
    '';

    strictDeps = true;
    nativeBuildInputs =
      [
        gn
        ninja
        python3
      ]
      ++ lib.optionals stdenv.hostPlatform.isDarwin [
        xcbuild
        cctools.libtool
        zlib
        fixDarwinDylibNames
      ];

    buildInputs =
      [
        expat
        fontconfig
        freetype
        harfbuzzFull
        icu
        libGL
        libjpeg
        libwebp
        libX11
      ]
      ++ lib.optionals enableVulkan [
        vulkan-headers
        vulkan-memory-allocator
      ];

    gnFlags =
      let
        cpu =
          {
            "x86_64" = "x64";
            "i686" = "x86";
            "arm" = "arm";
            "aarch64" = "arm64";
          }
          .${stdenv.hostPlatform.parsed.cpu.name};
      in
      # See https://github.com/mono/SkiaSharp/wiki/Building-on-Linux for how
      # mono skia is built.
      #
      # Also see https://skia.org/docs/user/build/.
      [
        "is_official_build=true" # Build in release mode
        # Don't use missing tools
        "skia_use_dng_sdk=false"
        # Use system dependencies
        "extra_cflags=[\"-I${harfbuzzFull.dev}/include/harfbuzz\", \"-DSKIA_C_DLL\"]"
        "cc=\"${stdenv.cc.targetPrefix}cc\""
        "cxx=\"${stdenv.cc.targetPrefix}c++\""
        "ar=\"${stdenv.cc.targetPrefix}ar\""
        "target_cpu=\"${cpu}\""
      ]
      ++ map (lib: "skia_use_system_${lib}=true") [
        "zlib"
        "libpng"
        "libwebp"
      ]
      ++ [
        "skia_use_wuffs=${lib.trivial.boolToString(enableWuffs)}"
        "skia_use_vulkan=${lib.trivial.boolToString(enableVulkan)}"
      ]
      ++ lib.optionals stdenv.hostPlatform.isDarwin [
        "skia_use_fontconfig=true"
        "skia_use_freetype=true"
        "skia_use_metal=true"
      ];

    env.NIX_LDFLAGS = lib.optionalString stdenv.hostPlatform.isDarwin "-lz";

    # Somewhat arbitrary, but similar to what other distros are doing
    installPhase = ''
      runHook preInstall

      # Libraries
      mkdir -p $out/lib
      cp *.so *.a *.dylib $out/lib

      # Includes
      pushd ../../include
      find . -name '*.h' -exec install -Dm644 {} $out/include/skia/{} \;
      popd
      pushd ../../modules
      find . -name '*.h' -exec install -Dm644 {} $out/include/skia/modules/{} \;
      popd

      # Pkg-config
      mkdir -p $out/lib/pkgconfig
      cat > $out/lib/pkgconfig/skia.pc <<'EOF'
      prefix=${placeholder "out"}
      exec_prefix=''${prefix}
      libdir=''${prefix}/lib
      includedir=''${prefix}/include/skia
      Name: skia
      Description: 2D graphic library for drawing text, geometries and images.
      URL: https://skia.org/
      Version: ${lib.versions.major finalAttrs.version}
      Libs: -L''${libdir} -lskia
      Cflags: -I''${includedir}
      EOF

      runHook postInstall
    '';

    preFixup = ''
      # Some skia includes are assumed to be under an include sub directory by
      # other includes
      for file in $(grep -rl '#include "include/' $out/include); do
        substituteInPlace "$file" \
          --replace-fail '#include "include/' '#include "'
      done
    '';

    passthru.tests.pkg-config = testers.testMetaPkgConfig finalAttrs.finalPackage;

    meta = {
      description = "2D graphic library for drawing text, geometries and images";
      homepage = "https://skia.org/";
      license = lib.licenses.bsd3;
      maintainers = with lib.maintainers; [ fgaz ];
      platforms = with lib.platforms; arm ++ aarch64 ++ x86 ++ x86_64;
      pkgConfigModules = [ "skia" ];
    };
})
