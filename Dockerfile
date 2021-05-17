FROM hexpm/elixir:1.11.1-erlang-23.3.4-alpine-3.13.3 AS bulid_stage

RUN apk update && \
    apk add git

RUN mkdir -p /buildroot
WORKDIR /buildroot
COPY . .

ENV MIX_ENV=prod

RUN mix local.rebar --force && \
    mix deps.get
RUN mix release


############################################################


FROM alpine:3.13.3

RUN apk update && \
    apk add ncurses-dev

COPY --from=bulid_stage /buildroot/_build/prod/rel/app  /srv/app
WORKDIR /srv/app
CMD ["./bin/app", "start"]
