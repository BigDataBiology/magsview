// This is called BEFORE your Elm app starts up
// 
// The value returned here will be passed as flags 
// into your `Shared.init` function.
export const flags = () => {

}

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
// FASTA cache: url -> Map<contigName, sequence>
const fastaCache = new Map();
// In-flight fetches to avoid duplicate requests
const fastaFetching = new Map();

function parseFasta(text) {
  const contigs = new Map();
  let currentName = null;
  const chunks = [];
  for (const line of text.split('\n')) {
    if (line.startsWith('>')) {
      if (currentName !== null) {
        contigs.set(currentName, chunks.join(''));
        chunks.length = 0;
      }
      currentName = line.slice(1).split(/\s/)[0];
    } else {
      chunks.push(line.trim());
    }
  }
  if (currentName !== null) {
    contigs.set(currentName, chunks.join(''));
  }
  return contigs;
}

// Decompress a gzipped response body and decode it as UTF-8 text
function gunzipText(response) {
  return new Response(response.body.pipeThrough(new DecompressionStream('gzip'))).text();
}

// URLs are built on the Elm side (Data.Info) and passed in the port payloads
async function fetchFasta(url) {
  if (fastaCache.has(url)) {
    return fastaCache.get(url);
  }
  if (fastaFetching.has(url)) {
    return fastaFetching.get(url);
  }
  const promise = (async () => {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to fetch FASTA: ${response.status}`);
    }
    const contigs = parseFasta(await gunzipText(response));
    fastaCache.set(url, contigs);
    return contigs;
  })().finally(() => fastaFetching.delete(url));
  fastaFetching.set(url, promise);
  return promise;
}

const complementMap = { A: 'T', T: 'A', G: 'C', C: 'G', a: 't', t: 'a', g: 'c', c: 'g', N: 'N', n: 'n' };

function reverseComplement(seq) {
  return seq.split('').reverse().map(c => complementMap[c] || 'N').join('');
}

const codonTable = {
  'TTT':'F','TTC':'F','TTA':'L','TTG':'L',
  'CTT':'L','CTC':'L','CTA':'L','CTG':'L',
  'ATT':'I','ATC':'I','ATA':'I','ATG':'M',
  'GTT':'V','GTC':'V','GTA':'V','GTG':'V',
  'TCT':'S','TCC':'S','TCA':'S','TCG':'S',
  'CAT':'H','CAC':'H','CAA':'Q','CAG':'Q',
  'CCT':'P','CCC':'P','CCA':'P','CCG':'P',
  'ACT':'T','ACC':'T','ACA':'T','ACG':'T',
  'GCT':'A','GCC':'A','GCA':'A','GCG':'A',
  'TAT':'Y','TAC':'Y','TAA':'*','TAG':'*',
  'TGT':'C','TGC':'C','TGA':'*','TGG':'W',
  'CGT':'R','CGC':'R','CGA':'R','CGG':'R',
  'AGT':'S','AGC':'S','AGA':'R','AGG':'R',
  'AAT':'N','AAC':'N','AAA':'K','AAG':'K',
  'GAT':'D','GAC':'D','GAA':'E','GAG':'E',
  'GGT':'G','GGC':'G','GGA':'G','GGG':'G'
};

const startCodonsTable11 = new Set(['TTG', 'CTG', 'ATT', 'ATC', 'ATA', 'ATG', 'GTG']);

function translateDNA(dna) {
  const protein = [];
  const upper = dna.toUpperCase();
  for (let i = 0; i + 2 < upper.length; i += 3) {
    const codon = upper.slice(i, i + 3);
    if (i === 0 && startCodonsTable11.has(codon)) {
      protein.push('M');
    } else {
      protein.push(codonTable[codon] || 'X');
    }
  }
  return protein.join('');
}

export const onReady = ({ app, env }) => {
  let sc = document.createElement('script');
  sc.setAttribute('src', "https://www.googletagmanager.com/gtag/js?id=G-60E4QKLHYR");
  sc.setAttribute('async', true);
  document.getElementById('google-injection-site').appendChild(sc);

  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-60E4QKLHYR');
  gtag('send', 'pageview');
  app.ports.updatePath.subscribe(function(path) {
      gtag('set', 'page', '/'+path);
      gtag('send', 'pageview');
  });

  app.ports.copyToClipboard.subscribe(function(args) {
    const { text, buttonId } = args;
    navigator.clipboard.writeText(text).then(function() {
      const btn = document.getElementById(buttonId);
      if (btn) {
        btn.textContent = '\u2713';
        btn.classList.add('copy-btn-copied');
        setTimeout(function() {
          btn.textContent = '\u{1F4CB}';
          btn.classList.remove('copy-btn-copied');
        }, 1500);
      }
    });
  });

  app.ports.downloadGeneFasta.subscribe(async function(request) {
    try {
      const { magId, fastaUrl, genes, format } = request;
      const contigs = await fetchFasta(fastaUrl);
      const lines = [];
      for (const gene of genes) {
        const contigSeq = contigs.get(gene.contig);
        if (!contigSeq) continue;
        let dna = contigSeq.slice(gene.start - 1, gene.end);
        if (gene.strand === '-') {
          dna = reverseComplement(dna);
        }
        const seq = format === 'faa' ? translateDNA(dna) : dna;
        const namePart = gene.preferredName || '-';
        const cogPart = gene.cogCategory || '-';
        const koPart = gene.keggKo.length > 0 ? ' ' + gene.keggKo.join(',') : '';
        const header = `>${gene.seqid} (predicted gene name: ${namePart}; COG class ${cogPart})${koPart}`;
        lines.push(header);
        // Wrap sequence at 80 characters
        for (let i = 0; i < seq.length; i += 80) {
          lines.push(seq.slice(i, i + 80));
        }
      }
      const content = lines.join('\n') + '\n';
      const ext = format === 'faa' ? 'faa' : 'fna';
      const blob = new Blob([content], { type: 'text/plain' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `${magId}.${ext}`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } catch (err) {
      console.error('Failed to generate FASTA download:', err);
    }
  });

  app.ports.requestGeneSequence.subscribe(async function(request) {
    // seqid is echoed back so Elm can drop replies for a gene that is no longer selected
    const { fastaUrl, contig, start, end, strand, seqid, skipTranslation } = request;
    try {
      const contigs = await fetchFasta(fastaUrl);
      const contigSeq = contigs.get(contig);
      if (!contigSeq) {
        app.ports.receiveGeneSequence.send({ seqid, error: `Contig ${contig} not found in FASTA` });
        return;
      }
      // Coordinates are 1-based
      let dna = contigSeq.slice(start - 1, end);
      if (strand === '-') {
        dna = reverseComplement(dna);
      }
      const protein = skipTranslation ? '' : translateDNA(dna);
      app.ports.receiveGeneSequence.send({ seqid, dna, protein });
    } catch (err) {
      app.ports.receiveGeneSequence.send({ seqid, error: err.message || String(err) });
    }
  });

  app.ports.requestRRnaGenes.subscribe(async function(request) {
    try {
      const { url } = request;
      const response = await fetch(url);
      if (!response.ok) {
        app.ports.receiveRRnaGenes.send({ error: `Failed to fetch rRNA data: ${response.status}` });
        return;
      }
      const text = await gunzipText(response);

      // FASTA headers are parsed on the Elm side (Barrnap.parseHeader)
      const headers = text.split('\n').filter(line => line.startsWith('>'));
      app.ports.receiveRRnaGenes.send({ headers });
    } catch (err) {
      app.ports.receiveRRnaGenes.send({ error: err.message || String(err) });
    }
  });
}
