export WITH_INSTALL=1
export BUILD_BASE_DIR=$PWD/../temp_build
export CONFIG_OPTIONS="--disable-shared"
export PKG_CONFIG_PATH="$BUILD_BASE_DIR/$BUILD_STYLE/local/lib/pkgconfig"
export UB_PRODUCTS="bin/dlsdump bin/gigextract bin/rifftree bin/gigdump lib/libgig.a"
export CFLAGS="-framework CoreFoundation"
export CXXFLAGS="$CFLAGS"
source $PROJECT_DIR/autoconf_builder.sh
