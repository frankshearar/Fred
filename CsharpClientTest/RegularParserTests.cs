using Fred.RegularExpression;
using NUnit.Framework;
using System.Linq;

namespace CsharpClient
{
    [TestFixture]
    public class RegularParserTests
    {
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
            Assert.That(matches.Any());
            Assert.AreEqual("a", matches.Single());
        }
    }
}
