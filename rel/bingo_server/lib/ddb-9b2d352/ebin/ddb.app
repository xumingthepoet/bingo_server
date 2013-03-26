{application,ddb,
             [{description,"AWS DynamoDB client"},
              {vsn,"9b2d352"},
              {registered,[ddb_sup]},
              {applications,[kernel,stdlib,crypto,ssl,ibrowse]},
              {env,[]},
              {modules,[ddb,ddb_aws,ddb_iam,ddb_util,ddb_xml]}]}.
