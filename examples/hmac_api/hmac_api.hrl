-author("Hypernumbers Ltd <gordon@hypernumbers.com>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Reference values for testing against Amazon documents                    %%%
%%%                                                                          %%%
%%% These need to be changed in production!                                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(schema, "AWS").
%% defines the prefix for headers to be included in the signature
-define(headerprefix, "x-amz-").
%% defines the date header
-define(dateheader, "x-amz-date").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Default values for defining a generic API                                %%%
%%%                                                                          %%%
%%% Only change these if you alter the canonicalisation                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-define(schema, "MOCHIAPI").
%%-define(headerprefix, "x-mochiapi-").
%%-define(dateheader, "x-mochiapi-date").

%% a couple of keys for testing
%% these are taken from the document
%% % http://docs.amazonwebservices.com/AmazonS3/latest/dev/index.html?RESTAuthentication.html
%% they are not valid keys!
-define(publickey,  "0PN5J17HBGZHT7JJ3X82").
-define(privatekey, "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o").


-record(hmac_signature,
        {
          method,
          contentmd5,
          contenttype,
          date,
          headers,
          resource
        }).
