FROM erlang:23.3.2-alpine AS bulid_stage

RUN apk update && \
    apk add git

RUN mkdir -p /buildroot
WORKDIR /buildroot
COPY . .
RUN rebar3 release


############################################################


FROM alpine:3.13

RUN apk update && \
    apk add ncurses-dev

COPY --from=bulid_stage /buildroot/_build/default/rel/app  /srv/app
WORKDIR /srv/app
CMD ["./bin/app", "foreground"]
