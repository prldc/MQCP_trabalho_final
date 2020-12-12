import GetOldTweets3 as got
import pandas as pd

df = pd.read_excel('Haddad_Fernando_potential_influencers*.xlsx')
usernames = ['cirogomes', 'geraldoalckmin', 'GuilhermeBoulos', 'Haddad_Fernando',
 'jairbolsonaro', 'LulaOficial', 'MarinaSilva']

def download_tweets(username):
    print(f"Downloading for {username}")
    tweetCriteria = got.manager.TweetCriteria().setUsername(username)\
                                               .setSince("2018-01-01")\
                                               .setUntil("2020-01-01")\

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    df = pd.DataFrame([tweet.__dict__ for tweet in tweets])
    print(df.shape)
    df.to_csv(f"{username}.csv", index=False)


for username in usernames:
    download_tweets(username)