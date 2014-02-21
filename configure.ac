AC_INIT([Haskell libNVVM bindings], [1.0.0], [selee@nvidia.com], [cuda])
AC_CONFIG_SRCDIR([Foreign/LibNVVM.hs])
AC_CONFIG_FILES([libnvvm.buildinfo])
AC_PROG_CC

AC_CANONICAL_TARGET

# Search the users PATH for the 'nvcc' complier. If it is found, we will add the
# nvvm include and library search directories relative to that.

AC_ARG_WITH([compiler], [  --with-compiler=HC      use HC as the Haskell compiler],
            [GHC=$withval],  [AC_PATH_PROG(GHC, ghc)])
AC_ARG_WITH([nvcc],     [  --with-nvcc=NVCC        use NVCC as the CUDA compiler],
            [NVCC=$withval], [AC_PATH_PROG(NVCC, nvcc, [], [$PATH:/usr/local/cuda/bin])])
AC_ARG_WITH([gcc],      [  --with-gcc=CC           use CC as the C compiler],
            [CC=$withval])


# If we detected nvcc, guess the location of nvvm relative to that. Otherwise,
# just pick /usr/local/cuda as a reasonable default.

if test "$NVCC" != ""; then
    cuda_prefix="$(dirname "$(dirname "$NVCC")")"
else
    echo "WARNING: Cannot find the CUDA compiler 'nvcc'. This is likely to cause problems."
    cuda_prefix="/usr/local/cuda"
fi

# Location of nvvm header and library

nvvm_prefix="${cuda_prefix}/nvvm"
nvvm_inc_path="${nvvm_prefix}/include"

case $target_os in
  darwin*)
    nvvm_lib_path="${nvvm_prefix}/lib"
    LDFLAGS+=" -Xlinker -rpath ${nvvm_lib_path} "
    ;;
  *)
    case $target_cpu in
      x86_64*)
        nvvm_lib_path="${nvvm_prefix}/lib64"
        ;;
      *)
        nvvm_lib_path="${nvvm_prefix}/lib"
        ;;
    esac
    ;;
esac

CPPFLAGS+=" -I${nvvm_inc_path} "
LDFLAGS+=" -L${nvvm_lib_path} "


# Make sure it was found

longerror='
********************************************************************************

The configuration process failed to locate your CUDA installation. Ensure that
you have installed both the developer driver and toolkit, available from:

  http://developer.nvidia.com/cuda-downloads

and make sure that "nvcc" is available in your PATH. Check the above output log
and run the command directly to ensure it can be located.

If you have a non-standard installation, you can add additional search paths
using --extra-include-dirs and --extra-lib-dirs. Note that 64-bit Linux flavours
often require both "lib64" and "lib" library paths, in that order.

********************************************************************************'

AC_CHECK_HEADERS([nvvm.h],        [], [AC_MSG_ERROR(could not find NVVM headers${longerror})])
AC_SEARCH_LIBS(nvvmVersion, nvvm, [], [AC_MSG_ERROR(could not find NVVM library@{longerror})])


# Populate the build info

nvvm_ccflags="$CPPFLAGS"
nvvm_ldflags="$LDGLAGS $LIBS "

AC_SUBST([nvvm_ccflags])
AC_SUBST([nvvm_ldflags])
AC_SUBST([nvvm_lib_path])
AC_OUTPUT

