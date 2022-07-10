FROM debian:stable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -yq install locales haskell-stack \
	&& stack upgrade --binary-only
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8
ENV LANG en_US.UTF-8
ENV STACK_ROOT /stack
WORKDIR /workspace
RUN stack build --only-dependencies
