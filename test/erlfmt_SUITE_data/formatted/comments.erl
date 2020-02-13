-module(comments).

%% Constants
% Minimum interval between health checks
-define(VERSION_CHECK_INTERVAL_MILLIS_DEFAULT, 10000).

-define(MAX_WRITE_FAILURES, 3).
