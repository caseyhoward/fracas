import { GraphQLResolveInfo } from 'graphql';
export type Maybe<T> = T | null;
export type RequireFields<T, K extends keyof T> = { [X in Exclude<keyof T, K>]?: T[X] } & { [P in K]-?: NonNullable<T[P]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string,
  String: string,
  Boolean: boolean,
  Int: number,
  Float: number,
};

export type BodyOfWater = {
   __typename?: 'BodyOfWater',
  id: Scalars['String'],
  neighboringCountries: Array<Scalars['String']>,
};

export type BodyOfWaterInput = {
  id: Scalars['String'],
  neighboringCountries: Array<Scalars['String']>,
};

export type Color = {
   __typename?: 'Color',
  red: Scalars['Int'],
  green: Scalars['Int'],
  blue: Scalars['Int'],
};

export type ColorInput = {
  red: Scalars['Int'],
  green: Scalars['Int'],
  blue: Scalars['Int'],
};

export type Country = {
   __typename?: 'Country',
  id: Scalars['String'],
  coordinates: Array<Point>,
  polygon: Array<Point>,
  waterEdges: Array<Segment>,
  center: Point,
  neighboringCountries: Array<Scalars['String']>,
  neighboringBodiesOfWater: Array<Scalars['String']>,
};

export type CountryInput = {
  id: Scalars['String'],
  coordinates: Array<PointInput>,
  polygon: Array<PointInput>,
  waterEdges: Array<SegmentInput>,
  center: PointInput,
  neighboringCountries: Array<Scalars['String']>,
  neighboringBodiesOfWater: Array<Scalars['String']>,
};

export type CountryTroopCounts = {
   __typename?: 'CountryTroopCounts',
  countryId: Scalars['String'],
  troopCount: Scalars['Int'],
};

export type CountryTroopCountsInput = {
  countryId: Scalars['String'],
  troopCount: Scalars['Int'],
};

export type Dimensions = {
   __typename?: 'Dimensions',
  width: Scalars['Int'],
  height: Scalars['Int'],
};

export type DimensionsInput = {
  width: Scalars['Int'],
  height: Scalars['Int'],
};

export type Game = {
   __typename?: 'Game',
  id: Scalars['String'],
  map: Map,
  players: Array<Player>,
  neutralCountryTroops: Array<CountryTroopCounts>,
  playerTurn: PlayerTurn,
};

export type GameInput = {
  id: Scalars['String'],
  mapId: Scalars['String'],
  players: Array<PlayerInput>,
  neutralCountryTroops: Array<CountryTroopCountsInput>,
  playerTurn: PlayerTurnInput,
};

export type InternetGame = {
   __typename?: 'InternetGame',
  game: Game,
  currentUserPlayerId: Scalars['String'],
};

export type InternetGameConfiguration = {
   __typename?: 'InternetGameConfiguration',
  id: Scalars['String'],
  players: Array<InternetGamePlayerConfiguration>,
  mapId: Scalars['String'],
  joinToken: Scalars['String'],
  currentUserPlayerId: Scalars['String'],
};

export type InternetGameOrConfiguration = InternetGameConfiguration | InternetGame;

export type InternetGamePlayerConfiguration = {
   __typename?: 'InternetGamePlayerConfiguration',
  color: Color,
  playerId: Scalars['String'],
  name: Scalars['String'],
};

export type Map = {
   __typename?: 'Map',
  id: Scalars['String'],
  name: Scalars['String'],
  countries: Array<Country>,
  bodiesOfWater: Array<BodyOfWater>,
  dimensions: Dimensions,
};

export type MapInput = {
  name: Scalars['String'],
  countries: Array<CountryInput>,
  bodiesOfWater: Array<BodyOfWaterInput>,
  dimensions: DimensionsInput,
};

export type Mutation = {
   __typename?: 'Mutation',
  createGame: Game,
  createInternetGame: Scalars['String'],
  createMap: Map,
  joinInternetGame: Scalars['String'],
  removePlayer: Game,
  saveGame: Game,
  saveInternetGame: Scalars['Boolean'],
  startInternetGame: Scalars['Boolean'],
  updateMapForInternetGame: Scalars['Boolean'],
  updatePlayerNameForInternetGame: Scalars['Boolean'],
  updatePlayerColorForInternetGame: Scalars['Boolean'],
};


export type MutationCreateGameArgs = {
  newGame: NewGameInput
};


export type MutationCreateMapArgs = {
  map: MapInput
};


export type MutationJoinInternetGameArgs = {
  joinGameToken: Scalars['String']
};


export type MutationRemovePlayerArgs = {
  playerToken: Scalars['String'],
  playerId: Scalars['String']
};


export type MutationSaveGameArgs = {
  game: GameInput
};


export type MutationSaveInternetGameArgs = {
  playerToken: Scalars['String'],
  game: GameInput
};


export type MutationStartInternetGameArgs = {
  playerToken: Scalars['String']
};


export type MutationUpdateMapForInternetGameArgs = {
  playerToken: Scalars['String'],
  mapId: Scalars['String']
};


export type MutationUpdatePlayerNameForInternetGameArgs = {
  name: Scalars['String'],
  playerToken: Scalars['String']
};


export type MutationUpdatePlayerColorForInternetGameArgs = {
  color: ColorInput,
  playerToken: Scalars['String']
};

export type NewGameInput = {
  mapId: Scalars['String'],
  players: Array<PlayerInput>,
  neutralCountryTroops: Array<CountryTroopCountsInput>,
  playerTurn: PlayerTurnInput,
};

export type Player = {
   __typename?: 'Player',
  id: Scalars['String'],
  name: Scalars['String'],
  countryTroopCounts: Array<CountryTroopCounts>,
  capitol?: Maybe<Scalars['String']>,
  color: Color,
  ports: Array<Scalars['String']>,
};

export type PlayerInput = {
  id: Scalars['String'],
  name: Scalars['String'],
  countryTroopCounts: Array<CountryTroopCountsInput>,
  capitol?: Maybe<Scalars['String']>,
  color: ColorInput,
  ports: Array<Scalars['String']>,
};

export type PlayerTurn = {
   __typename?: 'PlayerTurn',
  playerId: Scalars['String'],
  playerTurnStage: PlayerTurnStage,
  fromCountryId?: Maybe<Scalars['String']>,
  troopCount?: Maybe<Scalars['String']>,
};

export type PlayerTurnInput = {
  playerId: Scalars['String'],
  playerTurnStage: PlayerTurnStage,
  fromCountryId?: Maybe<Scalars['String']>,
  troopCount?: Maybe<Scalars['String']>,
};

export enum PlayerTurnStage {
  CapitolPlacement = 'CapitolPlacement',
  TroopPlacement = 'TroopPlacement',
  AttackAnnexOrPort = 'AttackAnnexOrPort',
  TroopMovement = 'TroopMovement',
  TroopMovementFromSelected = 'TroopMovementFromSelected',
  GameOver = 'GameOver'
}

export type Point = {
   __typename?: 'Point',
  x: Scalars['Int'],
  y: Scalars['Int'],
};

export type PointInput = {
  x: Scalars['Int'],
  y: Scalars['Int'],
};

export type Query = {
   __typename?: 'Query',
  map: Map,
  game: Game,
  internetGameOrConfiguration: InternetGameOrConfiguration,
  internetGame: InternetGame,
  maps: Array<Map>,
};


export type QueryMapArgs = {
  id: Scalars['String']
};


export type QueryGameArgs = {
  id: Scalars['String']
};


export type QueryInternetGameOrConfigurationArgs = {
  playerToken: Scalars['String']
};


export type QueryInternetGameArgs = {
  playerToken: Scalars['String']
};

export type Segment = {
   __typename?: 'Segment',
  point1: Point,
  point2: Point,
};

export type SegmentInput = {
  point1: PointInput,
  point2: PointInput,
};



export type ResolverTypeWrapper<T> = Promise<T> | T;

export type ResolverFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => Promise<TResult> | TResult;


export type StitchingResolver<TResult, TParent, TContext, TArgs> = {
  fragment: string;
  resolve: ResolverFn<TResult, TParent, TContext, TArgs>;
};

export type Resolver<TResult, TParent = {}, TContext = {}, TArgs = {}> =
  | ResolverFn<TResult, TParent, TContext, TArgs>
  | StitchingResolver<TResult, TParent, TContext, TArgs>;

export type SubscriptionSubscribeFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => AsyncIterator<TResult> | Promise<AsyncIterator<TResult>>;

export type SubscriptionResolveFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

export interface SubscriptionSubscriberObject<TResult, TKey extends string, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<{ [key in TKey]: TResult }, TParent, TContext, TArgs>;
  resolve?: SubscriptionResolveFn<TResult, { [key in TKey]: TResult }, TContext, TArgs>;
}

export interface SubscriptionResolverObject<TResult, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<any, TParent, TContext, TArgs>;
  resolve: SubscriptionResolveFn<TResult, any, TContext, TArgs>;
}

export type SubscriptionObject<TResult, TKey extends string, TParent, TContext, TArgs> =
  | SubscriptionSubscriberObject<TResult, TKey, TParent, TContext, TArgs>
  | SubscriptionResolverObject<TResult, TParent, TContext, TArgs>;

export type SubscriptionResolver<TResult, TKey extends string, TParent = {}, TContext = {}, TArgs = {}> =
  | ((...args: any[]) => SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>)
  | SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>;

export type TypeResolveFn<TTypes, TParent = {}, TContext = {}> = (
  parent: TParent,
  context: TContext,
  info: GraphQLResolveInfo
) => Maybe<TTypes>;

export type NextResolverFn<T> = () => Promise<T>;

export type DirectiveResolverFn<TResult = {}, TParent = {}, TContext = {}, TArgs = {}> = (
  next: NextResolverFn<TResult>,
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

/** Mapping between all available schema types and the resolvers types */
export type ResolversTypes = {
  Query: ResolverTypeWrapper<{}>,
  String: ResolverTypeWrapper<Scalars['String']>,
  Map: ResolverTypeWrapper<Map>,
  Country: ResolverTypeWrapper<Country>,
  Point: ResolverTypeWrapper<Point>,
  Int: ResolverTypeWrapper<Scalars['Int']>,
  Segment: ResolverTypeWrapper<Segment>,
  BodyOfWater: ResolverTypeWrapper<BodyOfWater>,
  Dimensions: ResolverTypeWrapper<Dimensions>,
  Game: ResolverTypeWrapper<Game>,
  Player: ResolverTypeWrapper<Player>,
  CountryTroopCounts: ResolverTypeWrapper<CountryTroopCounts>,
  Color: ResolverTypeWrapper<Color>,
  PlayerTurn: ResolverTypeWrapper<PlayerTurn>,
  PlayerTurnStage: PlayerTurnStage,
  InternetGameOrConfiguration: ResolversTypes['InternetGameConfiguration'] | ResolversTypes['InternetGame'],
  InternetGameConfiguration: ResolverTypeWrapper<InternetGameConfiguration>,
  InternetGamePlayerConfiguration: ResolverTypeWrapper<InternetGamePlayerConfiguration>,
  InternetGame: ResolverTypeWrapper<InternetGame>,
  Mutation: ResolverTypeWrapper<{}>,
  NewGameInput: NewGameInput,
  PlayerInput: PlayerInput,
  CountryTroopCountsInput: CountryTroopCountsInput,
  ColorInput: ColorInput,
  PlayerTurnInput: PlayerTurnInput,
  MapInput: MapInput,
  CountryInput: CountryInput,
  PointInput: PointInput,
  SegmentInput: SegmentInput,
  BodyOfWaterInput: BodyOfWaterInput,
  DimensionsInput: DimensionsInput,
  GameInput: GameInput,
  Boolean: ResolverTypeWrapper<Scalars['Boolean']>,
};

/** Mapping between all available schema types and the resolvers parents */
export type ResolversParentTypes = {
  Query: {},
  String: Scalars['String'],
  Map: Map,
  Country: Country,
  Point: Point,
  Int: Scalars['Int'],
  Segment: Segment,
  BodyOfWater: BodyOfWater,
  Dimensions: Dimensions,
  Game: Game,
  Player: Player,
  CountryTroopCounts: CountryTroopCounts,
  Color: Color,
  PlayerTurn: PlayerTurn,
  PlayerTurnStage: PlayerTurnStage,
  InternetGameOrConfiguration: ResolversParentTypes['InternetGameConfiguration'] | ResolversParentTypes['InternetGame'],
  InternetGameConfiguration: InternetGameConfiguration,
  InternetGamePlayerConfiguration: InternetGamePlayerConfiguration,
  InternetGame: InternetGame,
  Mutation: {},
  NewGameInput: NewGameInput,
  PlayerInput: PlayerInput,
  CountryTroopCountsInput: CountryTroopCountsInput,
  ColorInput: ColorInput,
  PlayerTurnInput: PlayerTurnInput,
  MapInput: MapInput,
  CountryInput: CountryInput,
  PointInput: PointInput,
  SegmentInput: SegmentInput,
  BodyOfWaterInput: BodyOfWaterInput,
  DimensionsInput: DimensionsInput,
  GameInput: GameInput,
  Boolean: Scalars['Boolean'],
};

export type BodyOfWaterResolvers<ContextType = any, ParentType extends ResolversParentTypes['BodyOfWater'] = ResolversParentTypes['BodyOfWater']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  neighboringCountries?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>,
};

export type ColorResolvers<ContextType = any, ParentType extends ResolversParentTypes['Color'] = ResolversParentTypes['Color']> = {
  red?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
  green?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
  blue?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
};

export type CountryResolvers<ContextType = any, ParentType extends ResolversParentTypes['Country'] = ResolversParentTypes['Country']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  coordinates?: Resolver<Array<ResolversTypes['Point']>, ParentType, ContextType>,
  polygon?: Resolver<Array<ResolversTypes['Point']>, ParentType, ContextType>,
  waterEdges?: Resolver<Array<ResolversTypes['Segment']>, ParentType, ContextType>,
  center?: Resolver<ResolversTypes['Point'], ParentType, ContextType>,
  neighboringCountries?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>,
  neighboringBodiesOfWater?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>,
};

export type CountryTroopCountsResolvers<ContextType = any, ParentType extends ResolversParentTypes['CountryTroopCounts'] = ResolversParentTypes['CountryTroopCounts']> = {
  countryId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  troopCount?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
};

export type DimensionsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Dimensions'] = ResolversParentTypes['Dimensions']> = {
  width?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
  height?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
};

export type GameResolvers<ContextType = any, ParentType extends ResolversParentTypes['Game'] = ResolversParentTypes['Game']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  map?: Resolver<ResolversTypes['Map'], ParentType, ContextType>,
  players?: Resolver<Array<ResolversTypes['Player']>, ParentType, ContextType>,
  neutralCountryTroops?: Resolver<Array<ResolversTypes['CountryTroopCounts']>, ParentType, ContextType>,
  playerTurn?: Resolver<ResolversTypes['PlayerTurn'], ParentType, ContextType>,
};

export type InternetGameResolvers<ContextType = any, ParentType extends ResolversParentTypes['InternetGame'] = ResolversParentTypes['InternetGame']> = {
  game?: Resolver<ResolversTypes['Game'], ParentType, ContextType>,
  currentUserPlayerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
};

export type InternetGameConfigurationResolvers<ContextType = any, ParentType extends ResolversParentTypes['InternetGameConfiguration'] = ResolversParentTypes['InternetGameConfiguration']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  players?: Resolver<Array<ResolversTypes['InternetGamePlayerConfiguration']>, ParentType, ContextType>,
  mapId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  joinToken?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  currentUserPlayerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
};

export type InternetGameOrConfigurationResolvers<ContextType = any, ParentType extends ResolversParentTypes['InternetGameOrConfiguration'] = ResolversParentTypes['InternetGameOrConfiguration']> = {
  __resolveType: TypeResolveFn<'InternetGameConfiguration' | 'InternetGame', ParentType, ContextType>
};

export type InternetGamePlayerConfigurationResolvers<ContextType = any, ParentType extends ResolversParentTypes['InternetGamePlayerConfiguration'] = ResolversParentTypes['InternetGamePlayerConfiguration']> = {
  color?: Resolver<ResolversTypes['Color'], ParentType, ContextType>,
  playerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  name?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
};

export type MapResolvers<ContextType = any, ParentType extends ResolversParentTypes['Map'] = ResolversParentTypes['Map']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  name?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  countries?: Resolver<Array<ResolversTypes['Country']>, ParentType, ContextType>,
  bodiesOfWater?: Resolver<Array<ResolversTypes['BodyOfWater']>, ParentType, ContextType>,
  dimensions?: Resolver<ResolversTypes['Dimensions'], ParentType, ContextType>,
};

export type MutationResolvers<ContextType = any, ParentType extends ResolversParentTypes['Mutation'] = ResolversParentTypes['Mutation']> = {
  createGame?: Resolver<ResolversTypes['Game'], ParentType, ContextType, RequireFields<MutationCreateGameArgs, 'newGame'>>,
  createInternetGame?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  createMap?: Resolver<ResolversTypes['Map'], ParentType, ContextType, RequireFields<MutationCreateMapArgs, 'map'>>,
  joinInternetGame?: Resolver<ResolversTypes['String'], ParentType, ContextType, RequireFields<MutationJoinInternetGameArgs, 'joinGameToken'>>,
  removePlayer?: Resolver<ResolversTypes['Game'], ParentType, ContextType, RequireFields<MutationRemovePlayerArgs, 'playerToken' | 'playerId'>>,
  saveGame?: Resolver<ResolversTypes['Game'], ParentType, ContextType, RequireFields<MutationSaveGameArgs, 'game'>>,
  saveInternetGame?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<MutationSaveInternetGameArgs, 'playerToken' | 'game'>>,
  startInternetGame?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<MutationStartInternetGameArgs, 'playerToken'>>,
  updateMapForInternetGame?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<MutationUpdateMapForInternetGameArgs, 'playerToken' | 'mapId'>>,
  updatePlayerNameForInternetGame?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<MutationUpdatePlayerNameForInternetGameArgs, 'name' | 'playerToken'>>,
  updatePlayerColorForInternetGame?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<MutationUpdatePlayerColorForInternetGameArgs, 'color' | 'playerToken'>>,
};

export type PlayerResolvers<ContextType = any, ParentType extends ResolversParentTypes['Player'] = ResolversParentTypes['Player']> = {
  id?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  name?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  countryTroopCounts?: Resolver<Array<ResolversTypes['CountryTroopCounts']>, ParentType, ContextType>,
  capitol?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>,
  color?: Resolver<ResolversTypes['Color'], ParentType, ContextType>,
  ports?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>,
};

export type PlayerTurnResolvers<ContextType = any, ParentType extends ResolversParentTypes['PlayerTurn'] = ResolversParentTypes['PlayerTurn']> = {
  playerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>,
  playerTurnStage?: Resolver<ResolversTypes['PlayerTurnStage'], ParentType, ContextType>,
  fromCountryId?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>,
  troopCount?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>,
};

export type PointResolvers<ContextType = any, ParentType extends ResolversParentTypes['Point'] = ResolversParentTypes['Point']> = {
  x?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
  y?: Resolver<ResolversTypes['Int'], ParentType, ContextType>,
};

export type QueryResolvers<ContextType = any, ParentType extends ResolversParentTypes['Query'] = ResolversParentTypes['Query']> = {
  map?: Resolver<ResolversTypes['Map'], ParentType, ContextType, RequireFields<QueryMapArgs, 'id'>>,
  game?: Resolver<ResolversTypes['Game'], ParentType, ContextType, RequireFields<QueryGameArgs, 'id'>>,
  internetGameOrConfiguration?: Resolver<ResolversTypes['InternetGameOrConfiguration'], ParentType, ContextType, RequireFields<QueryInternetGameOrConfigurationArgs, 'playerToken'>>,
  internetGame?: Resolver<ResolversTypes['InternetGame'], ParentType, ContextType, RequireFields<QueryInternetGameArgs, 'playerToken'>>,
  maps?: Resolver<Array<ResolversTypes['Map']>, ParentType, ContextType>,
};

export type SegmentResolvers<ContextType = any, ParentType extends ResolversParentTypes['Segment'] = ResolversParentTypes['Segment']> = {
  point1?: Resolver<ResolversTypes['Point'], ParentType, ContextType>,
  point2?: Resolver<ResolversTypes['Point'], ParentType, ContextType>,
};

export type Resolvers<ContextType = any> = {
  BodyOfWater?: BodyOfWaterResolvers<ContextType>,
  Color?: ColorResolvers<ContextType>,
  Country?: CountryResolvers<ContextType>,
  CountryTroopCounts?: CountryTroopCountsResolvers<ContextType>,
  Dimensions?: DimensionsResolvers<ContextType>,
  Game?: GameResolvers<ContextType>,
  InternetGame?: InternetGameResolvers<ContextType>,
  InternetGameConfiguration?: InternetGameConfigurationResolvers<ContextType>,
  InternetGameOrConfiguration?: InternetGameOrConfigurationResolvers,
  InternetGamePlayerConfiguration?: InternetGamePlayerConfigurationResolvers<ContextType>,
  Map?: MapResolvers<ContextType>,
  Mutation?: MutationResolvers<ContextType>,
  Player?: PlayerResolvers<ContextType>,
  PlayerTurn?: PlayerTurnResolvers<ContextType>,
  Point?: PointResolvers<ContextType>,
  Query?: QueryResolvers<ContextType>,
  Segment?: SegmentResolvers<ContextType>,
};


/**
 * @deprecated
 * Use "Resolvers" root object instead. If you wish to get "IResolvers", add "typesPrefix: I" to your config.
*/
export type IResolvers<ContextType = any> = Resolvers<ContextType>;
