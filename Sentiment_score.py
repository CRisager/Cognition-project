
# Basic
import numpy as np

# AFINN for sentiment analysis
from afinn import Afinn
afinn = Afinn()

# Sentiment
from scipy.stats import norm

# Sentiment score
def sentiment_score(data):
  #type(Data) = np.array eller kan nok også pandas series
  sentiment = []
  for message in data:
    try:
      sentiment += [afinn.score(message)]
    except:
      # Neutral er en placeholder og ændrer ikke i dataen
      message = "neutral"
      sentiment += [afinn.score(message)]
  return sentiment

