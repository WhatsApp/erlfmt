-module(comments).

%% Constants
-define(VERSION_CHECK_INTERVAL_MILLIS_DEFAULT, 10000). % Minimum interval between health checks
-define(MAX_WRITE_FAILURES, 3).
