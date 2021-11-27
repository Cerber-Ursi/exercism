class Transcriptor {

    toRna(sequence: string) {
        return Array
            .from(sequence)
            .map(this.toRnaSingle)
            .join('')
    }

    private toRnaSingle(nucleotide: string) {
        // Sanity check
        if (nucleotide.length !== 1) {
            throw new Error('toRnaSingle expects a single nucleotide')
        }
        // Transcription itself
        switch (nucleotide) {
            case 'C':
                return 'G'
            case 'G':
                return 'C'
            case 'T':
                return 'A'
            case 'A':
                return 'U'
            default:
                throw new Error('Invalid input DNA.')
        }
    }

}

export default Transcriptor