import React         from 'react';
import NavigationBar from '../../components/NavigationBar';
import SearchSidebar from '../../components/SearchSideBar';
import styles        from './Index.module.css';

const tags: string[] = [];

const Index = () => (
    <div>
        <NavigationBar />

        <div className={styles.PanelContainer}>
            <SearchSidebar tags={tags} />
        </div>
    </div>
);

export default Index
