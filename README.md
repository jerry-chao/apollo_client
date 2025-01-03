apollo
=====

An OTP application

Build
-----

    $ rebar3 compile


Spring Cloud Config
-----

Configuration through Spring Cloud Config server is supported. Add the following config:

```erlang
{spring_cloud_config, #{
    enabled => true,
    server_url => "http://config-server:8888",
    application => "your-app",
    profile => "dev",
    label => "master"
}}
```

The client will fetch configurations from both Apollo and Spring Cloud Config server.

Usage
-----

To get configuration values from Spring Cloud Config:

```erlang
% Get config with default value
Value = spring_cloud_config:get_config(<<"key">>, DefaultValue).

% Get config without default value
Value = spring_cloud_config:get_config(<<"key">>).
```

The client will automatically refresh configurations periodically.
