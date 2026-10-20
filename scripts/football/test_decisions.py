import unittest
from export_decisions import explicit_decision


class ShootoutMetadataTests(unittest.TestCase):
    def setUp(self):
        self.match = dict(home='Argentina', away='France', score='3-3 (p 4-2)',
                          result='draw', homeElo=2870, awayElo=2819)

    def test_explicit_shootout_preserves_rating_inputs(self):
        original = dict(self.match)
        self.assertEqual(explicit_decision(self.match), dict(winner='Argentina',
                         homePenalties=4, awayPenalties=2, decision='penalties'))
        self.assertEqual(self.match, original)

    def test_no_shootout_is_not_a_win(self):
        self.assertIsNone(explicit_decision(dict(self.match, score='3-3')))

    def test_home_away_orientation(self):
        self.assertEqual(explicit_decision(dict(self.match, score='3-3 (p 2-4)'))['winner'], 'France')

    def test_pending_nondraw_and_incomplete_shootouts(self):
        for changes in [dict(upcoming=True), dict(played=False),
                        dict(score='3-2 (p 4-2)'), dict(score='3-3 (p 4-4)')]:
            self.assertIsNone(explicit_decision(dict(self.match, **changes)))


if __name__ == '__main__':
    unittest.main()
