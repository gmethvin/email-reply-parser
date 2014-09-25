package net.methvin.email

import org.specs2.mutable.Specification

class EmailReplyParserSpec extends Specification {
  "EmailReplyParser" should {

    "parse simple bodies" in {
      val body =
        """this is an email with a correct -- signature.
          |
          |--
          |rick""".stripMargin
      parse(body, sigs = true) must_== body
      parse(body) must_== "this is an email with a correct -- signature."
    }

    "read bottom messages" in {
      val body =
        """Hi,
          |On Tue, 2011-03-01 at 18:02 +0530, Abhishek Kona wrote:
          |> Hi folks
          |>
          |> What is the best way to clear a Riak bucket of all key, values after
          |> running a test?
          |> I am currently using the Java HTTP API.
          |
          |You can list the keys for the bucket and call delete for each. Or if you
          |put the keys (and kept track of them in your test) you can delete them
          |one at a time (without incurring the cost of calling list first.)
          |
          |Something like:
          |
          |        String bucket = "my_bucket";
          |        BucketResponse bucketResponse = riakClient.listBucket(bucket);
          |        RiakBucketInfo bucketInfo = bucketResponse.getBucketInfo();
          |
          |        for(String key : bucketInfo.getKeys()) {
          |            riakClient.delete(bucket, key);
          |        }
          |
          |
          |would do it.
          |
          |See also
          |
          |http://wiki.basho.com/REST-API.html#Bucket-operations
          |
          |which says
          |
          |"At the moment there is no straightforward way to delete an entire
          |Bucket. There is, however, an open ticket for the feature. To delete all
          |the keys in a bucket, you’ll need to delete them all individually."
          |
          |>
          |> -Abhishek Kona
          |>
          |>
          |> _______________________________________________
          |> riak-users mailing list
          |> riak-users@lists.basho.com
          |> http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com
          |
          |
          |
          |
          |_______________________________________________
          |riak-users mailing list
          |riak-users@lists.basho.com
          |http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com""".stripMargin
      val parsed = parse(body)
      val fragments = EmailReplyParser(body).fragments
      fragments.map(_.quoted) must_== Seq(false, true, false, true, false, false)
      fragments.map(_.signature) must_== Seq(false, false, false, false, false, true)
      fragments.map(_.hidden) must_== Seq(false, false, false, true, true, true)
      parsed must contain("http://wiki.basho.com/REST-API.html#Bucket-operations")
      parsed must contain("You can list the keys for the bucket and call delete for each.")
    }

    "read top messages" in {
      val body =
        """Oh thanks.
          |
          |Having the function would be great.
          |
          |-Abhishek Kona
          |
          |On 01/03/11 7:07 PM, Russell Brown wrote:
          |> Hi,
          |> On Tue, 2011-03-01 at 18:02 +0530, Abhishek Kona wrote:
          |>> Hi folks
          |>>
          |>> What is the best way to clear a Riak bucket of all key, values after
          |>> running a test?
          |>> I am currently using the Java HTTP API.
          |> You can list the keys for the bucket and call delete for each. Or if you
          |> put the keys (and kept track of them in your test) you can delete them
          |> one at a time (without incurring the cost of calling list first.)
          |>
          |> Something like:
          |>
          |>          String bucket = "my_bucket";
          |>          BucketResponse bucketResponse = riakClient.listBucket(bucket);
          |>          RiakBucketInfo bucketInfo = bucketResponse.getBucketInfo();
          |>
          |>          for(String key : bucketInfo.getKeys()) {
          |>              riakClient.delete(bucket, key);
          |>          }
          |>
          |>
          |> would do it.
          |>
          |> See also
          |>
          |> http://wiki.basho.com/REST-API.html#Bucket-operations
          |>
          |> which says
          |>
          |> "At the moment there is no straightforward way to delete an entire
          |> Bucket. There is, however, an open ticket for the feature. To delete all
          |> the keys in a bucket, you’ll need to delete them all individually."
          |>
          |>> -Abhishek Kona
          |>>
          |>>
          |>> _______________________________________________
          |>> riak-users mailing list
          |>> riak-users@lists.basho.com
          |>> http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com
          |>
          |
          |
          |_______________________________________________
          |riak-users mailing list
          |riak-users@lists.basho.com
          |http://lists.basho.com/mailman/listinfo/riak-users_lists.basho.com""".stripMargin
      parse(body) must_==
        """Oh thanks.
          |
          |Having the function would be great.""".stripMargin
      EmailReplyParser(body).fragments must haveLength(5)
    }

    "capture date string" in {
      val body =
        """Awesome! I haven't had another problem with it.
          |
          |On Aug 22, 2011, at 7:37 PM, defunkt<reply@reply.github.com> wrote:
          |
          |> Loader seems to be working well""".stripMargin
      val fragments = EmailReplyParser(body).fragments
      fragments should haveLength(2)
      fragments(0).content must contain("Awesome")
      fragments(1).content must contain("On")
      fragments(1).content must contain("Loader")
      parse(body) must_== "Awesome! I haven't had another problem with it."
    }

    "handle complex bodies with one fragment" in {
      val body =
        """One: Here's what I've got.
          |
          |- This would be the first bullet point that wraps to the second line
          |to the next
          |- This is the second bullet point and it doesn't wrap
          |- This is the third bullet point and I'm having trouble coming up with enough
          |to say
          |- This is the fourth bullet point
          |
          |Two:
          |- Here is another bullet point
          |- And another one
          |
          |This is a paragraph that talks about a bunch of stuff. It goes on and on
          |for a while.""".stripMargin
      EmailReplyParser(body).fragments must haveLength(1)
      parse(body) must_== body
      parse(body, sigs = true) must_== body
    }

    "handle multiline reply headers" in {
      val body =
        """I get proper rendering as well.
          |
          |Sent from a magnificent torch of pixels
          |
          |On Dec 16, 2011, at 12:47 PM, Corey Donohoe
          |<reply@reply.github.com>
          |wrote:
          |
          |> Was this caching related or fixed already?  I get proper rendering here.
          |>
          |> ![](https://img.skitch.com/20111216-m9munqjsy112yqap5cjee5wr6c.jpg)
          |>
          |> ---
          |> Reply to this email directly or view it on GitHub:
          |> https://github.com/github/github/issues/2278#issuecomment-3182418""".stripMargin
      val fragments = EmailReplyParser(body).fragments
      parse(body) must_== "I get proper rendering as well."
      fragments(0).content must contain("I get")
      fragments(1).signature must beTrue
      fragments(1).content must contain("Sent from")
    }

    "read signature correctly" in {
      val body =
        """this is an email with a correct -- signature.
          |
          |--
          |rick""".stripMargin
      val fragments = EmailReplyParser(body).fragments
      fragments must haveLength(2)
      fragments.map(_.quoted) must_== Seq(false, false)
      fragments.map(_.signature) must_== Seq(false, true)
      fragments.map(_.hidden) must_== Seq(false, true)
      fragments(1).content must contain("--")
    }

    "deal with windows line endings" in {
      val body =
        """:+1:
          |
          |On Tue, Sep 25, 2012 at 8:59 AM, Chris Wanstrath
          |<notifications@github.com>wrote:
          |
          |> Steps 0-2 are in prod. Gonna let them sit for a bit then start cleaning up
          |> the old code with 3 & 4.
          |>
          |>
          |> Reply to this email directly or view it on GitHub.
          |>
          |>""".replace("\n", "\r\n").stripMargin
      parse(body) must_== ":+1:"
    }

    "handle \"Sent from my iPhone\"" in {
      val body =
        """Here is another email
          |
          |Sent from my iPhone""".stripMargin
      parse(body) must_== "Here is another email"
      parse(body, sigs = true) must_== body
    }

    "properly handle quote headers" in {
      val body =
        """Thank, this is really helpful.
          |
          |One outstanding question I had:
          |
          |Locally (on development), when I run...
          |
          |On Oct 1, 2012, at 11:55 PM, Dave Tapley wrote:
          |
          |> The good news is that I've found a much better query for lastLocation.
          |>
          |""".stripMargin
      val expectedResult =
        """Thank, this is really helpful.
          |
          |One outstanding question I had:
          |
          |Locally (on development), when I run...""".stripMargin
      parse(body) must_== expectedResult
      parse(body, sigs = true) must_== expectedResult
    }

    "handle partial quote headers" in {
      val body =
        """On your remote host you can run:
          |
          |     telnet 127.0.0.1 52698
          |
          |This should connect to TextMate (on your Mac, via the tunnel). If that
          |fails, the tunnel is not working.
          |
          |On 9 Jan 2014, at 2:47, George Plymale wrote:
          |
          |> I am having an odd issue wherein suddenly port forwarding stopped
          |> working in a particular scenario for me.  By default I have ssh set to
          |> use the following config (my ~/.ssh/config file):
          |> […]""".stripMargin
      val result = parse(body)
      result must contain("On your remote host you can run:")
      result must contain("telnet 127.0.0.1 52698")
      result must contain("This should connect to TextMate")
    }
  }

  def parse(s: String, sigs: Boolean = false): String = EmailReplyParser.parseReply(s, includeSignatures = sigs)
}
