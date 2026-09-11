const inputCsv = document.getElementById("inputCsv");
const nomeArquivo = document.getElementById("nomeArquivo");

inputCsv.addEventListener("change", () => {
    if (inputCsv.files.length > 0) {
        nomeArquivo.textContent = inputCsv.files[0].name;
    } else {
        nomeArquivo.textContent = "Nenhum arquivo selecionado";
    }
});

async function enviarParaAPI() {
    const input = document.getElementById('inputCsv');
    const status = document.getElementById('mensagemStatus');
    const areaGrafo = document.getElementById('areaGrafo');
    const imgGrafo = document.getElementById('imagemGrafo');
    
    // Novas variáveis da Estatística
    const areaEstatistica = document.getElementById('areaEstatistica');
    const imgEstatistica = document.getElementById('imagemEstatistica');

    if (input.files.length === 0){
        status.innerText = "Erro, por favor coloque o arquivo .csv antes de clicar em enviar";
        status.style.color = "#c0392b";
        return;
    }

    status.innerText = "Enviando dados para processamento... aguarde.";
    status.style.color = "#2980b9"; 
    areaGrafo.style.display = "none";
    areaEstatistica.style.display = "none";

    const dadosFormulario = new FormData();
    dadosFormulario.append("arquivo_usuario", input.files[0]);

    try {
        const resposta = await fetch("/analise-redes-biologicas/api/processar", {
            method: "POST",
            body: dadosFormulario
        });
        
        if (!resposta.ok) {
            throw new Error("Erro na comunicação com o servidor R.");
        }
        
        const dados_retornados = await resposta.json();

        fazerDownload(dados_retornados.arquivo1, "RME_resultado_vertices.csv");
        fazerDownload(dados_retornados.arquivo2, "RME_resultado_arestas.csv");

        // Renderiza a rede original
        imgGrafo.src = "data:image/png;base64," + dados_retornados.grafo;
        areaGrafo.style.display = "block";

        // Preenche os dados Estatísticos (se o vetor vier vazio, exibe "Nenhum")
        const est = dados_retornados.estatisticas_dados;
        document.getElementById('txtCellsP95').innerText = (est.cells_p95 && est.cells_p95.length > 0) ? est.cells_p95.join(", ") : "Nenhum";
        document.getElementById('txtSignalP95').innerText = (est.signal_p95 && est.signal_p95.length > 0) ? est.signal_p95.join(", ") : "Nenhum";
        document.getElementById('txtCellsOut').innerText = (est.cells_outliers && est.cells_outliers.length > 0) ? est.cells_outliers.join(", ") : "Nenhum";
        document.getElementById('txtSignalOut').innerText = (est.signal_outliers && est.signal_outliers.length > 0) ? est.signal_outliers.join(", ") : "Nenhum";

        // Renderiza os Histogramas
        imgEstatistica.src = "data:image/png;base64," + dados_retornados.grafico_estatistico;
        areaEstatistica.style.display = "block";

        status.innerText = "Processamento concluído com sucesso!";
        status.style.color = "#27ae60"; 

    } catch (erro) {
        status.innerText = "Falha ao conectar com o algoritmo. Verifique se o servidor R está rodando.";
        status.style.color = "#c0392b"; 
        console.error(erro);
    }
}

function fazerDownload(conteudoTexto, nomeDoArquivo) {
    const blob = new Blob([conteudoTexto], { type: 'text/csv' });
    const link = document.createElement('a');
    link.href = window.URL.createObjectURL(blob);
    link.download = nomeDoArquivo;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}