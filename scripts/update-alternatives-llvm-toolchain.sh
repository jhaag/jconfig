#!/usr/bin/env bash

#     --slave /usr/bin/$1 $1 /usr/bin/$1-\${version} \\

# install llvm XX (see directions here: https://apt.llvm.org/)
#wget https://apt.llvm.org/llvm.sh
#chmod +x llvm.sh
#sudo ./llvm.sh XX all

VERSION_USED_TO_CONFIGURE_SCRIPT=19
DEFAULT_PRIORITY=100

bold=$(tput bold)
normal=$(tput sgr0)

function print_usage_and_exit {
    if [ -n "$1" ]; then
        EXIT_CODE=$1
    else
        EXIT_CODE=0
    fi

    echo "Use ${bold}update-alternatives${normal} to manage installed ${bold}llvm${normal}/${bold}clang${normal} binaries; be sure to install llvm/clang (${bold}see directions here${normal}: https://apt.llvm.org/)."
    echo
    echo "${bold}Usage${normal}: $0 [${bold}-v${normal} <TOOLCHAIN-VERSION>] [${bold}-p${normal} <update-alternatives PRIORITY>]"
    echo
    echo "${bold}Defaults${normal}:"
    echo "- Toolchain Version (used to configure the script):     ${VERSION_USED_TO_CONFIGURE_SCRIPT}"
    echo "- Priority:                                             ${DEFAULT_PRIORITY}"
    exit $EXIT_CODE
}

function register_llvm_toolchain_version {
    local version=$1
    local priority=$2

    # NOTE: some of the following binaries aren't prefixed with llvm or clang, but we choose to pin them to [llvm-config] since
    # they might not relate to C++ specifically.
    #
    # NOTE: The following binaries are not included in the [update-alternatives] invocation since they aren't utilized (or have common names)
    #    --slave   /usr/bin/amdgpu-arch                     amdgpu-arch                     /usr/bin/amdgpu-arch-${version} \
    #    --slave   /usr/bin/count                           count                           /usr/bin/count-${version} \
    #    --slave   /usr/bin/FileCheck                       FileCheck                       /usr/bin/FileCheck-${version} \
    #    --slave   /usr/bin/modularize                      modularize                      /usr/bin/modularize-${version} \
    #    --slave   /usr/bin/not                             not                             /usr/bin/not-${version} \
    #    --slave   /usr/bin/nvptx-arch                      nvptx-arch                      /usr/bin/nvptx-arch-${version} \
    update-alternatives \
         --verbose \
        --install /usr/bin/llvm-config                     llvm-config                     /usr/bin/llvm-config-${version} ${priority} \
        --slave   /usr/bin/llvm-addr2line                  llvm-addr2line                  /usr/bin/llvm-addr2line-${version} \
        --slave   /usr/bin/llvm-ar                         llvm-ar                         /usr/bin/llvm-ar-${version} \
        --slave   /usr/bin/llvm-as                         llvm-as                         /usr/bin/llvm-as-${version} \
        --slave   /usr/bin/llvm-bcanalyzer                 llvm-bcanalyzer                 /usr/bin/llvm-bcanalyzer-${version} \
        --slave   /usr/bin/llvm-bitcode-strip              llvm-bitcode-strip              /usr/bin/llvm-bitcode-strip-${version} \
        --slave   /usr/bin/llvm-cat                        llvm-cat                        /usr/bin/llvm-cat-${version} \
        --slave   /usr/bin/llvm-cfi-verify                 llvm-cfi-verify                 /usr/bin/llvm-cfi-verify-${version} \
        --slave   /usr/bin/llvm-cov                        llvm-cov                        /usr/bin/llvm-cov-${version} \
        --slave   /usr/bin/llvm-c-test                     llvm-c-test                     /usr/bin/llvm-c-test-${version} \
        --slave   /usr/bin/llvm-cvtres                     llvm-cvtres                     /usr/bin/llvm-cvtres-${version} \
        --slave   /usr/bin/llvm-cxxdump                    llvm-cxxdump                    /usr/bin/llvm-cxxdump-${version} \
        --slave   /usr/bin/llvm-cxxfilt                    llvm-cxxfilt                    /usr/bin/llvm-cxxfilt-${version} \
        --slave   /usr/bin/llvm-cxxmap                     llvm-cxxmap                     /usr/bin/llvm-cxxmap-${version} \
        --slave   /usr/bin/llvm-debuginfo-analyzer         llvm-debuginfo-analyzer         /usr/bin/llvm-debuginfo-analyzer-${version} \
        --slave   /usr/bin/llvm-debuginfod                 llvm-debuginfod                 /usr/bin/llvm-debuginfod-${version} \
        --slave   /usr/bin/llvm-debuginfod-find            llvm-debuginfod-find            /usr/bin/llvm-debuginfod-find-${version} \
        --slave   /usr/bin/llvm-diff                       llvm-diff                       /usr/bin/llvm-diff-${version} \
        --slave   /usr/bin/llvm-dis                        llvm-dis                        /usr/bin/llvm-dis-${version} \
        --slave   /usr/bin/llvm-dlltool                    llvm-dlltool                    /usr/bin/llvm-dlltool-${version} \
        --slave   /usr/bin/llvm-dwarfdump                  llvm-dwarfdump                  /usr/bin/llvm-dwarfdump-${version} \
        --slave   /usr/bin/llvm-dwarfutil                  llvm-dwarfutil                  /usr/bin/llvm-dwarfutil-${version} \
        --slave   /usr/bin/llvm-dwp                        llvm-dwp                        /usr/bin/llvm-dwp-${version} \
        --slave   /usr/bin/llvm-exegesis                   llvm-exegesis                   /usr/bin/llvm-exegesis-${version} \
        --slave   /usr/bin/llvm-extract                    llvm-extract                    /usr/bin/llvm-extract-${version} \
        --slave   /usr/bin/llvm-gsymutil                   llvm-gsymutil                   /usr/bin/llvm-gsymutil-${version} \
        --slave   /usr/bin/llvm-ifs                        llvm-ifs                        /usr/bin/llvm-ifs-${version} \
        --slave   /usr/bin/llvm-jitlink                    llvm-jitlink                    /usr/bin/llvm-jitlink-${version} \
        --slave   /usr/bin/llvm-jitlink-executor           llvm-jitlink-executor           /usr/bin/llvm-jitlink-executor-${version} \
        --slave   /usr/bin/llvm-lib                        llvm-lib                        /usr/bin/llvm-lib-${version} \
        --slave   /usr/bin/llvm-libtool-darwin             llvm-libtool-darwin             /usr/bin/llvm-libtool-darwin-${version} \
        --slave   /usr/bin/llvm-link                       llvm-link                       /usr/bin/llvm-link-${version} \
        --slave   /usr/bin/llvm-lipo                       llvm-lipo                       /usr/bin/llvm-lipo-${version} \
        --slave   /usr/bin/llvm-lto                        llvm-lto                        /usr/bin/llvm-lto-${version} \
        --slave   /usr/bin/llvm-lto2                       llvm-lto2                       /usr/bin/llvm-lto2-${version} \
        --slave   /usr/bin/llvm-mc                         llvm-mc                         /usr/bin/llvm-mc-${version} \
        --slave   /usr/bin/llvm-mca                        llvm-mca                        /usr/bin/llvm-mca-${version} \
        --slave   /usr/bin/llvm-ml                         llvm-ml                         /usr/bin/llvm-ml-${version} \
        --slave   /usr/bin/llvm-modextract                 llvm-modextract                 /usr/bin/llvm-modextract-${version} \
        --slave   /usr/bin/llvm-mt                         llvm-mt                         /usr/bin/llvm-mt-${version} \
        --slave   /usr/bin/llvm-nm                         llvm-nm                         /usr/bin/llvm-nm-${version} \
        --slave   /usr/bin/llvm-objcopy                    llvm-objcopy                    /usr/bin/llvm-objcopy-${version} \
        --slave   /usr/bin/llvm-objdump                    llvm-objdump                    /usr/bin/llvm-objdump-${version} \
        --slave   /usr/bin/llvm-omp-device-info            llvm-omp-device-info            /usr/bin/llvm-omp-device-info-${version} \
        --slave   /usr/bin/llvm-omp-kernel-replay          llvm-omp-kernel-replay          /usr/bin/llvm-omp-kernel-replay-${version} \
        --slave   /usr/bin/llvm-opt-report                 llvm-opt-report                 /usr/bin/llvm-opt-report-${version} \
        --slave   /usr/bin/llvm-otool                      llvm-otool                      /usr/bin/llvm-otool-${version} \
        --slave   /usr/bin/llvm-pdbutil                    llvm-pdbutil                    /usr/bin/llvm-pdbutil-${version} \
        --slave   /usr/bin/llvm-PerfectShuffle             llvm-PerfectShuffle             /usr/bin/llvm-PerfectShuffle-${version} \
        --slave   /usr/bin/llvm-profdata                   llvm-profdata                   /usr/bin/llvm-profdata-${version} \
        --slave   /usr/bin/llvm-profgen                    llvm-profgen                    /usr/bin/llvm-profgen-${version} \
        --slave   /usr/bin/llvm-ranlib                     llvm-ranlib                     /usr/bin/llvm-ranlib-${version} \
        --slave   /usr/bin/llvm-rc                         llvm-rc                         /usr/bin/llvm-rc-${version} \
        --slave   /usr/bin/llvm-readelf                    llvm-readelf                    /usr/bin/llvm-readelf-${version} \
        --slave   /usr/bin/llvm-readobj                    llvm-readobj                    /usr/bin/llvm-readobj-${version} \
        --slave   /usr/bin/llvm-readtapi                   llvm-readtapi                   /usr/bin/llvm-readtapi-${version} \
        --slave   /usr/bin/llvm-reduce                     llvm-reduce                     /usr/bin/llvm-reduce-${version} \
        --slave   /usr/bin/llvm-remarkutil                 llvm-remarkutil                 /usr/bin/llvm-remarkutil-${version} \
        --slave   /usr/bin/llvm-rtdyld                     llvm-rtdyld                     /usr/bin/llvm-rtdyld-${version} \
        --slave   /usr/bin/llvm-sim                        llvm-sim                        /usr/bin/llvm-sim-${version} \
        --slave   /usr/bin/llvm-size                       llvm-size                       /usr/bin/llvm-size-${version} \
        --slave   /usr/bin/llvm-split                      llvm-split                      /usr/bin/llvm-split-${version} \
        --slave   /usr/bin/llvm-stress                     llvm-stress                     /usr/bin/llvm-stress-${version} \
        --slave   /usr/bin/llvm-strings                    llvm-strings                    /usr/bin/llvm-strings-${version} \
        --slave   /usr/bin/llvm-strip                      llvm-strip                      /usr/bin/llvm-strip-${version} \
        --slave   /usr/bin/llvm-symbolizer                 llvm-symbolizer                 /usr/bin/llvm-symbolizer-${version} \
        --slave   /usr/bin/llvm-tblgen                     llvm-tblgen                     /usr/bin/llvm-tblgen-${version} \
        --slave   /usr/bin/llvm-tli-checker                llvm-tli-checker                /usr/bin/llvm-tli-checker-${version} \
        --slave   /usr/bin/llvm-undname                    llvm-undname                    /usr/bin/llvm-undname-${version} \
        --slave   /usr/bin/llvm-windres                    llvm-windres                    /usr/bin/llvm-windres-${version} \
        --slave   /usr/bin/llvm-xray                       llvm-xray                       /usr/bin/llvm-xray-${version} \
        --slave   /usr/bin/analyze-build                   analyze-build                   /usr/bin/analyze-build-${version} \
        --slave   /usr/bin/asan_symbolize                  asan_symbolize                  /usr/bin/asan_symbolize-${version} \
        --slave   /usr/bin/bugpoint                        bugpoint                        /usr/bin/bugpoint-${version} \
        --slave   /usr/bin/c-index-test                    c-index-test                    /usr/bin/c-index-test-${version} \
        --slave   /usr/bin/diagtool                        diagtool                        /usr/bin/diagtool-${version} \
        --slave   /usr/bin/dsymutil                        dsymutil                        /usr/bin/dsymutil-${version} \
        --slave   /usr/bin/find-all-symbols                find-all-symbols                /usr/bin/find-all-symbols-${version} \
        --slave   /usr/bin/hmaptool                        hmaptool                        /usr/bin/hmaptool-${version} \
        --slave   /usr/bin/hwasan_symbolize                hwasan_symbolize                /usr/bin/hwasan_symbolize-${version} \
        --slave   /usr/bin/intercept-build                 intercept-build                 /usr/bin/intercept-build-${version} \
        --slave   /usr/bin/ld64.lld                        ld64.lld                        /usr/bin/ld64.lld-${version} \
        --slave   /usr/bin/ld.lld                          ld.lld                          /usr/bin/ld.lld-${version} \
        --slave   /usr/bin/llc                             llc                             /usr/bin/llc-${version} \
        --slave   /usr/bin/lld                             lld                             /usr/bin/lld-${version} \
        --slave   /usr/bin/lldb                            lldb                            /usr/bin/lldb-${version} \
        --slave   /usr/bin/lldb-argdumper                  lldb-argdumper                  /usr/bin/lldb-argdumper-${version} \
        --slave   /usr/bin/lldb-dap                        lldb-dap                        /usr/bin/lldb-dap-${version} \
        --slave   /usr/bin/lldb-instr                      lldb-instr                      /usr/bin/lldb-instr-${version} \
        --slave   /usr/bin/lldb-server                     lldb-server                     /usr/bin/lldb-server-${version} \
        --slave   /usr/bin/lld-link                        lld-link                        /usr/bin/lld-link-${version} \
        --slave   /usr/bin/lli                             lli                             /usr/bin/lli-${version} \
        --slave   /usr/bin/lli-child-target                lli-child-target                /usr/bin/lli-child-target-${version} \
        --slave   /usr/bin/obj2yaml                        obj2yaml                        /usr/bin/obj2yaml-${version} \
        --slave   /usr/bin/opt                             opt                             /usr/bin/opt-${version} \
        --slave   /usr/bin/pp-trace                        pp-trace                        /usr/bin/pp-trace-${version} \
        --slave   /usr/bin/sancov                          sancov                          /usr/bin/sancov-${version} \
        --slave   /usr/bin/sanstats                        sanstats                        /usr/bin/sanstats-${version} \
        --slave   /usr/bin/scan-build                      scan-build                      /usr/bin/scan-build-${version} \
        --slave   /usr/bin/scan-build-py                   scan-build-py                   /usr/bin/scan-build-py-${version} \
        --slave   /usr/bin/scan-view                       scan-view                       /usr/bin/scan-view-${version} \
        --slave   /usr/bin/split-file                      split-file                      /usr/bin/split-file-${version} \
        --slave   /usr/bin/UnicodeNameMappingGenerator     UnicodeNameMappingGenerator     /usr/bin/UnicodeNameMappingGenerator-${version} \
        --slave   /usr/bin/verify-uselistorder             verify-uselistorder             /usr/bin/verify-uselistorder-${version} \
        --slave   /usr/bin/wasm-ld                         wasm-ld                         /usr/bin/wasm-ld-${version} \
        --slave   /usr/bin/yaml2obj                        yaml2obj                        /usr/bin/yaml2obj-${version} \
        --slave   /usr/bin/yaml-bench                      yaml-bench                      /usr/bin/yaml-bench-${version}


    update-alternatives \
         --verbose \
        --install /usr/bin/clang                           clang                           /usr/bin/clang-${version} ${priority} \
        --slave   /usr/bin/clang++                         clang++                         /usr/bin/clang++-${version}  \
        --slave   /usr/bin/clang-apply-replacements        clang-apply-replacements        /usr/bin/clang-apply-replacements-${version}  \
        --slave   /usr/bin/clang-change-namespace          clang-change-namespace          /usr/bin/clang-change-namespace-${version}  \
        --slave   /usr/bin/clang-check                     clang-check                     /usr/bin/clang-check-${version} \
        --slave   /usr/bin/clang-cl                        clang-cl                        /usr/bin/clang-cl-${version} \
        --slave   /usr/bin/clang-cpp                       clang-cpp                       /usr/bin/clang-cpp-${version} \
        --slave   /usr/bin/clangd                          clangd                          /usr/bin/clangd-${version} \
        --slave   /usr/bin/clang-doc                       clang-doc                       /usr/bin/clang-doc-${version} \
        --slave   /usr/bin/clang-extdef-mapping            clang-extdef-mapping            /usr/bin/clang-extdef-mapping-${version}  \
        --slave   /usr/bin/clang-format                    clang-format                    /usr/bin/clang-format-${version}  \
        --slave   /usr/bin/clang-format-diff               clang-format-diff               /usr/bin/clang-format-diff-${version}  \
        --slave   /usr/bin/clang-include-cleaner           clang-include-cleaner           /usr/bin/clang-include-cleaner-${version} \
        --slave   /usr/bin/clang-include-fixer             clang-include-fixer             /usr/bin/clang-include-fixer-${version} \
        --slave   /usr/bin/clang-linker-wrapper            clang-linker-wrapper            /usr/bin/clang-linker-wrapper-${version} \
        --slave   /usr/bin/clang-move                      clang-move                      /usr/bin/clang-move-${version} \
        --slave   /usr/bin/clang-offload-bundler           clang-offload-bundler           /usr/bin/clang-offload-bundler-${version} \
        --slave   /usr/bin/clang-offload-packager          clang-offload-packager          /usr/bin/clang-offload-packager-${version} \
        --slave   /usr/bin/clang-pseudo                    clang-pseudo                    /usr/bin/clang-pseudo-${version} \
        --slave   /usr/bin/clang-query                     clang-query                     /usr/bin/clang-query-${version} \
        --slave   /usr/bin/clang-refactor                  clang-refactor                  /usr/bin/clang-refactor-${version} \
        --slave   /usr/bin/clang-rename                    clang-rename                    /usr/bin/clang-rename-${version} \
        --slave   /usr/bin/clang-reorder-fields            clang-reorder-fields            /usr/bin/clang-reorder-fields-${version} \
        --slave   /usr/bin/clang-repl                      clang-repl                      /usr/bin/clang-repl-${version} \
        --slave   /usr/bin/clang-scan-deps                 clang-scan-deps                 /usr/bin/clang-scan-deps-${version} \
        --slave   /usr/bin/clang-tblgen                    clang-tblgen                    /usr/bin/clang-tblgen-${version} \
        --slave   /usr/bin/clang-tidy                      clang-tidy                      /usr/bin/clang-tidy-${version} \
        --slave   /usr/bin/clang-tidy-diff.py              clang-tidy-diff.py              /usr/bin/clang-tidy-diff-${version}.py \
        --slave   /usr/bin/git-clang-format                git-clang-format                /usr/bin/git-clang-format-${version} \
        --slave   /usr/bin/run-clang-tidy                  run-clang-tidy                  /usr/bin/run-clang-tidy-${version} \
        --slave   /usr/bin/run-clang-tidy.py               run-clang-tidy.py               /usr/bin/run-clang-tidy-${version}.py

}

while getopts 'hv:p:' flag; do
    case "${flag}" in
      h) print_usage_and_exit ;; # default exit with 0
      v) SUPPLIED_VERSION="${OPTARG}" ;;
      p) SUPPLIED_PRIORITY="${OPTARG}" ;;
      *) print_usage_and_exit 1
    esac
done

if [ -z "$SUPPLIED_VERSION" ]; then
    VERSION=$VERSION_USED_TO_CONFIGURE_SCRIPT
else
    VERSION=$SUPPLIED_VERSION
fi

if [ "$VERSION" -ne "$VERSION_USED_TO_CONFIGURE_SCRIPT" ]; then
    echo "NOTE: The requested version ($VERSION) doesn't match the version used to write this script ($VERSION_USED_TO_CONFIGURE_SCRIPT)."
fi

if [ -z "$SUPPLIED_PRIORITY" ]; then
    PRIORITY=$DEFAULT_PRIORITY
else
    PRIORITY=$SUPPLIED_PRIORITY
fi

register_llvm_toolchain_version $VERSION $PRIORITY
