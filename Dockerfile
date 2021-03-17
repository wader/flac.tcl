FROM alpine:3.13.2
RUN apk add tcl ffmpeg flac
COPY flac.tcl test.sh ./
RUN ./test.sh ./flac.tcl
ENTRYPOINT ["./flac.tcl"]
