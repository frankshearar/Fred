using Fred.RegularExpression;
using NUnit.Framework;
using System;
using System.Linq;

namespace CsharpClientTest
{
    [TestFixture]
    public class RegularParserTests
    {
        [TestCase("aa")]
        [TestCase("aaa")]
        [TestCase("aaaa")]
        public void AtLeastProcessesExcessRepetitions(string input)
        {
            var p = RegularParser.Token('a').AtLeast(2);
            AssertParses(p, input);
        }

        [TestCase("")]
        [TestCase("a")]
        public void AtLeastRequiresInput(string input)
        {
            var p = RegularParser.Token('a').AtLeast(2);
            AssertDoesNotParse(p, input);
        }

        [Test]
        public void AtLeastRequiresNonNegativeNumber()
        {
            Assert.Throws<ArgumentException>(() => RegularParser.Token('a').AtLeast(-1));
        }

        [TestCase("")]
        [TestCase("a")]
        [TestCase("aa")]
        public void AtLeastWithZeroRepsActsLikeStar(string input)
        {
            var p = RegularParser.Token('a').AtLeast(0);
            AssertParses(p, input);
        }


        [TestCase("aaa")]
        [TestCase("aaaa")]
        public void AtMostDoesntParseLotsOfReps(string input)
        {
            var p = RegularParser.Token('a').AtMost(2);
            // Matches returns an list of IEnumerable<char>. Cast<>
            // helps compiler realise that IEnumerable<char> is nearly
            // equal to string.
            var matches = p.Match(input).Select(chars => new string(chars.ToArray()));
            Assert.False(matches.Any(s => s == input));
        }

        [TestCase("a")]
        [TestCase("aa")]
        public void AtMostParsesLowerReps(string input)
        {
            var p = RegularParser.Token('a').AtMost(2);
            AssertParses(p, input);
        }

        [Test]
        public void AtMostParsesNoReps()
        {
            var p = RegularParser.Token('a').AtMost(2);
            Assert.IsEmpty(p.Match("").ToList());
            AssertParses(p, "");
        }

        [Test]
        public void AtMostRequiresNonNegativeNumber()
        {
            Assert.Throws<ArgumentException>(() => RegularParser.Token('a').AtMost(-1));
        }

        [Test]
        public void CanCreateAlternateParser()
        {
            var p = RegularParser.Token('a').Or(RegularParser.Token('b'));
            var matches = p.Match("abc").ToList();
            Assert.AreEqual("a", matches[0]);
            Assert.AreEqual("b", matches[1]);
            Assert.AreEqual(2, matches.Count());
        }

        [Test]
        public void CanCreateEmptyParser()
        {
            var p = RegularParser.Eps();
            Assert.IsFalse(p.Match("abc").Any());
        }

        [Test]
        public void CanCreateFailingParser()
        {
            var p = RegularParser.Empty();
            Assert.IsFalse(p.Match("abc").Any());
        }

        [Test]
        public void CanCreateKleeneStarParser()
        {
            var p = RegularParser.Token('a').Star();
            var matches = p.Match("abaabbaaa").ToList();
            Assert.AreEqual("a", matches[0]);
            Assert.AreEqual("aa", matches[1]);
            Assert.AreEqual("aaa", matches[2]);
        }

        [Test]
        public void CanCreateSequenceParser()
        {
            var p = RegularParser.Token('a').Then(RegularParser.Token('b'));
            var matches = p.Match("abab").ToList();
            Assert.AreEqual("ab", matches[0]);
            Assert.AreEqual("ab", matches[1]);
            Assert.AreEqual(2, matches.Count);
        }

        [Test]
        public void CanCreateTokenParser()
        {
            var p = RegularParser.Token('a');
            var matches = p.Match("abc").ToList();
            Assert.IsNotEmpty(matches);
            Assert.AreEqual("a", matches.Single());
        }

        [Test]
        public void CountDemandsNonNegativeMinCount()
        {
            Assert.Throws<ArgumentException>(() => RegularParser.Token('a').Count(-1, 5));
        }

        [Test]
        public void CountDemandsMaxGreaterThanMin()
        {
            Assert.Throws<ArgumentException>(() => RegularParser.Token('a').Count(5, 1));
        }

        [Test]
        public void CountPermitsExactRepCount()
        {
            var p = RegularParser.Token('a').Count(3);
            AssertDoesNotParse(p, "aa");
            AssertParses(p, "aaa");
            var matches = p.Match("aaaa").Select(chars => new string(chars.ToArray()));
            Assert.That(matches.All(s => s == "aaa"));
        }

        [Test]
        public void CountWithMinEqualToMaxUsesMin()
        {
            var p = RegularParser.Token('a').Count(3,3);
            AssertDoesNotParse(p, "aa");
            AssertParses(p, "aaa");
            AssertDoesNotParse(p, "aaaa");
        }

        [Test]
        public void CountReturnsRepeatedParser()
        {
            var p = RegularParser.Token('a').Count(3, 5);
            AssertDoesNotParse(p, "aa");
            var matches = p.Match("aaaaaa");
            Assert.That(matches.All(s => 3 <= s.Count() && s.Count() <= 5));

            AssertParses(p, "aaa");
            AssertParses(p, "aaaa");
            AssertParses(p, "aaaaa");
        }

        [Test]
        public void AlphaAcceptsEnglishCharacters()
        {
            var p = RegularParser.Alpha.Count(3);
            AssertParses(p, "abc");
            AssertParses(p, "xyz");

            AssertDoesNotParse(p, "123");
        }

        [Test]
        public void ClassDefinesCharacterClass()
        {
            var p = RegularParser.Class("abc").Star();
            AssertParses(p, "aaa");
            AssertParses(p, "cba");

            AssertDoesNotParse(p, "xyz");
        }

        [Test]
        public void NumAcceptsDigits()
        {
            var p = RegularParser.Num.Count(10);
            AssertParses(p, "0123456789");

            AssertDoesNotParse(p, "abcdefghij");
        }

        private void AssertDoesNotParse(RegularParser<char> p, string input)
        {
            Assert.False(p.Recognise(input));
        }

        private void AssertParses(RegularParser<char> p, string input)
        {
            Assert.That(p.Recognise(input));
        }
    }
}
