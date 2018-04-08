import os

from sqlalchemy import create_engine
from jinja2 import Template
from pprint import pprint

from fabric.api import task

SEARCH_QUERIES_TMPL = """
select
    starttime, endtime, database, querytxt
from stl_query
join pg_user on (pg_user.usesysid = stl_query.userid)
where
    usename='{{ username }}'
    {% if start_date %}
    and starttime >= '{{ start_date }}'
        {% if end_date %}
        and endtime <= '{{ end_date }}'
        {% endif %}
    {% endif %}
order by starttime desc
limit {{ limit }}
"""

@task
def search_queries(username, **kwargs):
    """
    Show query statement by a specific user

    Args:
      username:   the user that you want to query
      start_date: Start date of the queries (default: None)
      end_date:   End date of the queries (default: now)
      limit:      how many queries you want to display. (default: 100)
      conn_str:   connect string to redshfit in sqlalchemy format.
                  if not specified, use env 'RS_CONNECT'.
    """
    limit      = int(kwargs.get('limit', 100))
    start_date = kwargs.get('start_date')
    end_date   = kwargs.get('end_date')
    conn_str   = kwargs.get('conn_str', os.environ.get('RS_CONNECT'))
    engine = create_engine(conn_str)

    sql = Template(SEARCH_QUERIES_TMPL).render(
        username=username,
        start_date=start_date,
        end_date=end_date,
        limit=limit,
        )

    with engine.connect() as conn:
        rs = conn.execute(sql)
        for row in rs:
            running_time = row['endtime'] - row['starttime']
            query_text = row['querytxt'].strip()
            print "Date:", row['starttime']
            print "Running Time:", running_time
            print "Database:", row['database']
            print "Query Text:\n{}\n\n".format(query_text)
