FROM debian:buster

RUN apt-get update \
 && apt-get install -y \
    curl \
    netbase \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

ADD dist /app

# Include "shush" to decode KMS_ENCRYPTED_STUFF
RUN curl -sL -o /usr/local/bin/shush \
  https://github.com/realestate-com-au/shush/releases/download/v1.3.4/shush_linux_amd64 \
  && chmod +x /usr/local/bin/shush
ENTRYPOINT ["/usr/local/bin/shush", "exec", "--"]

# buildkite artifacts won't be executable, so need this to ensure it can be run
RUN chmod a+x /app/capture-pd-alerts-to-trello-exe

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

RUN chown -R appuser:appuser /app

USER appuser

CMD echo 'Starting Capturing PagerDuty alerts as Trello cards ..' && /app/capture-pd-alerts-to-trello-exe
