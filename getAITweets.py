#!/usr/bin/python
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
import re
import sys
import json
import pdb
import dateutil.parser
from pytz import timezone
import pytz
import time

ckey = '3WnVCoNN979c7FMrTwQYY48UW'
consumer_secret = '7bbQmCapE6uT1cAdfCxcUzcTnvbAgEIOBEU1SSZxjySRgUxVeR'
access_token_key = '23326159-Zxu0HLpxFuTNK6IrC8eSleMrGhgvTlFYjpSvyUCV0'
access_token_secret = 'IXPFWATDrJnnkNN9AWPBb398Zxdf7NNm91PamJUuyVUvQ'


# The consumer keys can be found on your application's Details
# page located at https://dev.twitter.com/apps (under "OAuth settings")
CONSUMER_KEY = '3WnVCoNN979c7FMrTwQYY48UW'
CONSUMER_SECRET = ''

# The access tokens can be found on your applications's Details
# page located at https://dev.twitter.com/apps (located
# under "Your access token")
ACCESS_TOKEN = ''
ACCESS_TOKEN_SECRET = ''

sgtz = timezone('US/Eastern')
utc = pytz.timezone('UTC')

KEYWORDS = [
        u'bird flu',
		u'avian influenza',
		u'avian flu',
		u'poultry disease'
        ]
regex = re.compile('|'.join(KEYWORDS).lower())
linenum_re = re.compile(r'([A-Z][A-Z]\d+)')
retweets_re = re.compile(r'^RT\s')

enc = lambda x: x.encode('latin', errors='ignore')

class StdOutListener(StreamListener):
    def on_data(self, data):
        try:
            tweet = json.loads(data)
        #pdb.set_trace()
            if not tweet:
                tweet = ''
            if not 'user' in tweet:
                print('No user data - ignoring tweet.')
                return True

            user = str(enc(tweet['user']['name']))
            text = str(enc(tweet['text']))


        # ignore text that doesn't contain one of the keywords
            matches = re.search(regex, text.lower())
            if not matches:
                return True

        # ignore retweets
            if re.search(retweets_re, text):
                return True

            location = enc(tweet['user']['location'])
            source = enc(tweet['source'])
            d = dateutil.parser.parse(enc(tweet['created_at']))



        # localize time
            d_tz = utc.normalize(d)
            localtime = d.astimezone(sgtz)
            tmstr = localtime.strftime("%Y%m%d-%H:%M:%S")

        # append the hourly tweet file
            with open('tweets-%s.data' % tmstr.split(':')[0], 'a+') as f:
                f.write(data)

        # is this a geocoded tweet?
            geo = tweet['geo']
            if geo and geo['type'] == 'Point':
            # collect location of mrt station
                coords = geo['coordinates']
                ln = re.search(linenum_re, text)
                if ln:
                    with open('mrt_station_locations.csv', 'a+') as mrtgeo:
                        print("Found geo coords for MRT Station (%s) '%s': (%f, %f)\n" %
                                (ln.group(), matches.group(), coords[1], coords[0]))
                        mrtgeo.write("%f\t%f\t%s\t%s\n" %
                                (coords[1], coords[0], matches.group(), ln.group()))

        # print summary of tweet
            print('%s\n%s\n%s\n%s\n%s\n\n ----------------\n' % (user, location, source, tmstr, text))
            return True
		
        except Exception as e:
            print("failed")
            time.sleep(5)
            pass

    def on_error(self, status):
        print('status: %s' % status)



if __name__ == '__main__':
    l = StdOutListener()
    auth = OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
    auth.set_access_token(ACCESS_TOKEN, ACCESS_TOKEN_SECRET)

    stream = Stream(auth, l, timeout=60)

    print("Listening to filter stream...")

    stream.filter(track=KEYWORDS)