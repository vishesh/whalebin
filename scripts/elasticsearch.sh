#curl -XDELETE "http://localhost:9200/whalebin"

curl -XPUT "http://localhost:9200/whalebin" -d'
{
  "settings": {
    "index": {
      "number_of_shards": 1,
      "number_of_replicas": 1
    },
    "analysis": {
      "filter": {
        "hashtag_filter": {
          "type": "word_delimiter",
          "type_table": [
            "# => ALPHA"
          ]
        }
      },
      "analyzer": {
        "hashtag_analyzer": {
          "type": "custom",
          "tokenizer": "whitespace",
          "filter": [
            "lowercase",
            "hashtag_filter"
          ]
        }
      }
    }
  },
  "mappings": {
    "paste": {
      "_id": {
          "path": "url"
      },
      "properties": {
        "url": {
            "type": "string",
            "index": "not_analyzed",
            "store": "true"
        },
        "title": {
          "type": "string",
          "analyzer": "hashtag_analyzer"
        },
        "description": {
          "type": "string",
          "analyzer": "hashtag_analyzer"
        }
      }
    }
  }
}'
