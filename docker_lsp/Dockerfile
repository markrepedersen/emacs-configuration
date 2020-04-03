FROM ubuntu:18.04

RUN apk update && apk add alpine-sdk cmake make clang clang-static clang-dev llvm-dev llvm-static \
	&& git clone --depth=1 --recursive https://github.com/MaskRay/ccls \
	&& cd ccls \
	&& cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release \
	&& cmake --build Release --target install \
	&& curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh \
	&& rustup update && \
	rustup component add rls \
	rust-analysis \
	rust-src
