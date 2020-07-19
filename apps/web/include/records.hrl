%% @doc a web request
%% sometimes we care about dt (datetime)
-record(request, {
  url :: web_request:url(),
  dt,
  brand,
  bike,
  product_info_location
}).
