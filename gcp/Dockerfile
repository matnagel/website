FROM debian:stable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install locales haskell-stack \
	&& stack upgrade --binary-only
WORKDIR /workspace
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8
ENV LANG en_US.UTF-8
COPY stack.yaml package.yaml /workspace/
RUN mkdir app src test && stack build --only-dependencies
