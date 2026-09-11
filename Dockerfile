FROM rocker/shiny:4.6.1

# Create a non-root user
RUN useradd -m -s /sbin/nologin shinyuser

# Install system dependencies (if needed)
RUN apt-get update && apt-get install -y \
    # add any system deps your R packages need \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/shinyuser/app

# Copy renv files first (for better Docker layer caching)
COPY --chown=shinyuser:shinyuser renv.lock .
COPY --chown=shinyuser:shinyuser .Rprofile .

# Restore packages using renv while still as root
# (renv needs to write to the package cache)
RUN R -e "renv::restore()"

# Copy your app files
COPY --chown=shinyuser:shinyuser app/ .

# Copy your 15MB data file
COPY --chown=shinyuser:shinyuser data/processed/ ./data/

# Switch to non-root user
USER shinyuser

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
