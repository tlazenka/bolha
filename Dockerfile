FROM rust:1.45.0-stretch

RUN rustup component add rustfmt

ENV APP_HOME /app
WORKDIR $APP_HOME

COPY ./Cargo.toml .
COPY ./src/lib.rs ./src/lib.rs

RUN cargo fetch

COPY . .

CMD ["cargo", "test"]
