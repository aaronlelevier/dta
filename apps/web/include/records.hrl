%% @doc a web request
%% sometimes we care about dt (datetime)
-record(request, {
  url :: web_request:url(),
  dt,
  brand,
  bike
}).
