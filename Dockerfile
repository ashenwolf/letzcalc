FROM swipl
COPY app /app
COPY public /public
EXPOSE 8080

ENTRYPOINT ["swipl"]
CMD ["/app/server.pl", "--user=daemon", "--fork=false", "--port=8080"]

