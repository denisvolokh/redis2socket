FROM correl/erlang:17

RUN apt-get install -y vim

VOLUME /app
WORKDIR /app

EXPOSE 7878
