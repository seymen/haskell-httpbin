FROM haskell:8.6.5

EXPOSE 3000

ADD . .
RUN stack build

CMD stack exec haskell-httpbin-exe
