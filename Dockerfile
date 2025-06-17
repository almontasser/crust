FROM rust:latest
RUN apt-get update -y && apt-get install -y build-essential
WORKDIR /usr/src/crust
COPY . .
RUN chmod +x runtests.sh tests/runtests
CMD ["./runtests.sh"]
