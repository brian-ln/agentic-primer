#!/usr/bin/env bun

/**
 * Signal CLI - Manage and monitor signal detectors via Entangled Actor pattern
 *
 * Commands:
 *   signal list                       List all signals
 *   signal stats                      Show signal statistics
 *   signal fetch <url|id>             Fetch signal from YouTube
 *   signal playlist <id>              Fetch signals from YouTube playlist
 *   signal search <query>             Search YouTube videos
 *   signal get <id>                   Show signal details
 *   signal query <keyword>            Search captured signals
 */

import { ActorClient } from "../actors/actor-client.ts";

const TARGET_SIGNALS = "primer.signals.youtube";

// Helper to show help
function showHelp() {
  console.log("Signal CLI (Entangled Actor Mode)");
  console.log("\nUsage: signal <command> [args]");
  console.log("\nCommands:");
  console.log("  list                              List all captured signals");
  console.log("  stats                             Show captured signals statistics");
  console.log("  fetch <videoId>                   Fetch and process a YouTube video");
  console.log("  playlist <playlistId>             Fetch all videos from a playlist");
  console.log("  channels-add <id> [--name N]      Add a YouTube channel to monitor");
  console.log("  channels-list                     List monitored channels");
  console.log("  channels-remove <id>              Remove a monitored channel");
  console.log("  channels-scan <id> [--full]       Scan a channel for new videos");
  console.log("  search <query>                    Search YouTube videos (not captured yet)");
  console.log("  get <signalId>                    Show detailed signal properties");
  console.log("  query <keyword>                   Search title/transcript of captured signals");
  console.log("\nGlobal Options:");
  console.log("  --json                            Output results in JSON format");
  console.log("  --help                            Show this help message");
}

async function main() {
  const args = process.argv.slice(2);
  const client = new ActorClient();

  if (args.length === 0 || args.includes("--help")) {
    showHelp();
    return;
  }

  const command = args[0];
  const jsonMode = args.includes("--json");

  try {
    switch (command) {
      case "list": {
        const res = await client.send(TARGET_SIGNALS, "query", {});
        if (!res.success) throw new Error(res.error);
        
        if (jsonMode) {
          console.log(JSON.stringify(res.data.signals, null, 2));
        } else {
          console.log("\nCaptured Signals:");
          console.log("─".repeat(80));
          const signals = res.data.signals as any[];
          if (signals.length === 0) console.log("No signals captured yet.");
          signals.forEach(s => {
            const date = s.capturedAt ? new Date(s.capturedAt).toISOString().split('T')[0] : "unknown";
            console.log(`${s.id.padEnd(25)} ${date} ${s.title}`);
          });
          console.log(`\nTotal: ${res.data.total}`);
        }
        break;
      }

      case "stats": {
        const res = await client.send(TARGET_SIGNALS, "get_stats", {});
        if (!res.success) throw new Error(res.error);
        console.log(JSON.stringify(res.data, null, 2));
        break;
      }

      case "fetch": {
        const videoId = args[1];
        if (!videoId) throw new Error("Usage: signal fetch <videoId>");
        console.log(`Fetching video ${videoId}...`);
        const res = await client.send(TARGET_SIGNALS, "fetch_signals", { videoIds: [videoId] });
        if (!res.success) throw new Error(res.error);
        console.log("Fetch result:", JSON.stringify(res.data, null, 2));
        break;
      }

      case "playlist": {
        const playlistId = args[1];
        if (!playlistId) throw new Error("Usage: signal playlist <playlistId>");
        console.log(`Fetching playlist ${playlistId}...`);
        const res = await client.send(TARGET_SIGNALS, "fetch_from_playlist", { playlistId });
        if (!res.success) throw new Error(res.error);
        console.log("Playlist result:", JSON.stringify(res.data, null, 2));
        break;
      }

      case "channels-add": {
        const channelId = args[1];
        const nameIdx = args.indexOf("--name");
        const name = nameIdx >= 0 ? args[nameIdx + 1] : undefined;
        if (!channelId) throw new Error("Usage: signal channels-add <id> [--name N]");
        const res = await client.send("primer.signals.youtube.channel-monitor", "add_channel", { channelId, name });
        if (!res.success) throw new Error(res.error);
        console.log("Channel added:", JSON.stringify(res.data, null, 2));
        break;
      }

      case "channels-list": {
        const res = await client.send("primer.signals.youtube.channel-monitor", "list_channels", {});
        if (!res.success) throw new Error(res.error);
        if (jsonMode) {
          console.log(JSON.stringify(res.data, null, 2));
        } else {
          console.log("\nMonitored Channels:");
          console.log("─".repeat(80));
          (res.data as any[]).forEach(c => {
            console.log(`${c.id.padEnd(35)} ${c.title}`);
          });
        }
        break;
      }

      case "channels-remove": {
        const id = args[1];
        if (!id) throw new Error("Usage: signal channels-remove <id>");
        const res = await client.send("primer.signals.youtube.channel-monitor", "remove_channel", { id });
        if (!res.success) throw new Error(res.error);
        console.log(`Removed channel: ${id}`);
        break;
      }

      case "channels-scan": {
        const id = args[1];
        const full = args.includes("--full");
        if (!id) throw new Error("Usage: signal channels-scan <id> [--full]");
        const res = await client.send("primer.signals.youtube.channel-monitor", "scan_channel", { id, full });
        if (!res.success) throw new Error(res.error);
        console.log("Scan result:", JSON.stringify(res.data, null, 2));
        break;
      }

      case "search": {
        const query = args.slice(1).join(" ");
        if (!query) throw new Error("Usage: signal search <query>");
        const res = await client.send(TARGET_SIGNALS, "search_videos", { query });
        if (!res.success) throw new Error(res.error);
        console.log(JSON.stringify(res.data, null, 2));
        break;
      }

      case "query": {
        const keyword = args[1];
        if (!keyword) throw new Error("Usage: signal query <keyword>");
        const res = await client.send(TARGET_SIGNALS, "query", { keyword });
        if (!res.success) throw new Error(res.error);
        console.log(JSON.stringify(res.data, null, 2));
        break;
      }

      case "get": {
        const id = args[1];
        if (!id) throw new Error("Usage: signal get <signalId>");
        // Redirect to generic graph show for details
        const res = await client.send("primer.graph", "get_node", { id });
        if (!res.success) throw new Error(res.error);
        console.log(JSON.stringify(res.data, null, 2));
        break;
      }

      default:
        console.error(`Unknown command: ${command}`);
        showHelp();
        process.exit(1);
    }
  } catch (e: any) {
    console.error("Error:", e.message);
    process.exit(1);
  }
}

main().catch(console.error);
